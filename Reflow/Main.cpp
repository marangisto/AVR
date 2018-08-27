#include <AVR/Main.h>
#include <Arduino/Pins.h>
#include <AVR/ADC.h>
#include <AVR/UART.h>
#include <AVR/Timer.h>
#include <stdio.h>
#include <string.h>
#include <stdlib.h>

template <class T> const T& max(const T& a, const T& b) { return (a<b) ? b : a; }
template <class T> const T& min(const T& a, const T& b) { return (a<b) ? a : b; }

typedef D13 led;
typedef D3 ssr_a;
typedef D2 ssr_b;
typedef D12 aref_on;

static volatile float g_p = 0, g_i = 0, g_d = 0;

class pid_reg_t
{
public:
    pid_reg_t(float dt, float kp, float ki, float kd)
        : m_dt(dt), m_kp(kp), m_ki(ki), m_kd(kd), m_int(0), m_err(0)
    {}

    float compute(float rt, float yt)
    {
        float err = rt - yt;
        float p = m_kp * err;                           // proportional term

        m_int += err * m_dt;
        m_int = min(max<double>(m_int, -10.), 10.);     // clamping

        float i = m_ki * m_int;                         // integral term
        float d = m_kd * (err - m_err) / m_dt;          // derivative term

        m_err = err;

        g_p = p; g_i = i; g_d = d;

        return min(max<double>(p + i + d, .0), 1.);
    }

private:
    float m_dt, m_kp, m_ki, m_kd, m_int, m_err;
};


template<class PIN>
class slow_pwm
{
public:
    static void update()
    {
        if (m_i == 0)           // consider new duty-cycle at beginning of period only
            m_x = m_x_;

        PIN::write(m_x && m_x >= m_i++);
    }

    static void set(float x)    // [0..1] range
    {
        m_x_ = static_cast<uint8_t>(min(max(.0, 255. * x), 255.));
    }

private:
    static volatile uint8_t m_x_;
    static uint8_t m_x;
    static uint8_t m_i;
};

template<class PIN> volatile uint8_t slow_pwm<PIN>::m_x_ = 0;
template<class PIN> uint8_t slow_pwm<PIN>::m_x = 0;
template<class PIN> uint8_t slow_pwm<PIN>::m_i = 0;

typedef slow_pwm<ssr_a> pwm_a;

typedef timer_t<0> clock;

static volatile uint16_t clock_ticks = 0;

static void clock_isr()
{
    static bool tick_tock = false;

    if (tick_tock)
        ++clock_ticks;

    tick_tock = !tick_tock;
    pwm_a::update();
}

static float clock_time(uint16_t ticks) { return 20e-3 * ticks; }

static const float vref = 1.346;

template<int CH>
class thermocouple
{
public:
    static float voltage()
    {
        static const uint8_t n = 2;     // discard some readings
        static const uint8_t m = 16;    // average over readings
        float v = 0;

        for (uint8_t i = 0; i < n; ++i)
            adc::read<CH, adc_ref_aref>();

        uint16_t t = 0;

        for (uint8_t i = 0; i < m; ++i)
            t += adc::read<CH, adc_ref_aref>();

        v = vref * static_cast<float>(t) / (m * 1023);

        return v;
    }
};

typedef thermocouple<2> couple_a;
typedef thermocouple<1> couple_b;

static const float ambient = 20;
static volatile float temp_a = 0;
static volatile float temp_b = 0;
static volatile float yt = 0;
static volatile float rt = ambient;
static volatile float ut = 0;

static pid_reg_t pid_reg(0.1, 0.1, 0.01, 0.1);

typedef timer_t<1> control;

static void control_isr()
{
    led::toggle();  // show we are alive

    float v_a = couple_a::voltage();
    float v_b = couple_b::voltage();

    temp_a = v_a / 5e-3;
    temp_b = v_b / 5e-3;
    yt = 0.5 * (temp_a + temp_b);

    ut = pid_reg.compute(rt, yt);

    pwm_a::set(ut);
}

void setup()
{
    led::setup();
    ssr_a::setup();
    ssr_b::setup();
    aref_on::setup();
    aref_on::set();
    adc::setup<128>();
    UART::setup<115200>();
    printf("Marangisto Reflow 1.0\n");

    control::setup<ctc_mode, top_ocra>();
    control::clock_select<1024>();
    control::output_compare_register<channel_a>() = 1561; // 0.1s period
    control::isr_oca(control_isr);
    control::enable_oca();

    clock::setup<ctc_mode, top_ocra>();
    clock::clock_select<1024>();
    clock::output_pin<channel_a>::setup();
    clock::compare_output_mode<channel_a, toggle_on_compare_match>();
    clock::output_compare_register<channel_a>() = 155; // 10ms period
    clock::isr_oca(clock_isr);
    clock::enable_oca();

    sei();
}

// 0 = not started, 1 = ramp, 2 = soak, 3 = ramp, 4 = reflow, 5 = cool, 6 = stop
//
enum state_t { init, ramp1, soak, ramp2, reflow, cool, stop };

static const char *show_state(state_t s)
{
    switch (s)
    {
    case init: return "init";
    case ramp1: return "ramp1";
    case soak: return "soak";
    case ramp2: return "ramp2";
    case reflow: return "reflow";
    case cool: return "cool";
    case stop: return "stop";
    default: return "????";
    }
}

void loop()
{
    static uint8_t i = 0;
    static state_t state = init;
    static float soak_start = 0;
    static float reflow_start = 0;

    if ((i & 0x5) == 0)
        ssr_b::toggle();

    float t = clock_time(clock_ticks);

    switch (state)
    {
    case init:
        if (t > 10)
        {
            rt = 150;
            state = ramp1;
        }
        break;
    case ramp1:
        if (yt > 145)
        {
            soak_start = t;
            state = soak;
        }
        break;
    case soak:
        if (t - soak_start > 90)
        {
            rt = 220;
            state = ramp2;
        }
        break;
    case ramp2:
        if (yt > 215)
        {
            reflow_start = t;
            state = reflow;
        }
        break;
    case reflow:
        if (t - reflow_start > 30)
        {
            rt = ambient;
            state = cool;
        }
        break;
    case cool:
        if (yt < 100)
        {
            state = stop;
        }
        break;
    default:
        rt = ambient;
    }

    if (state < stop)
        printf("%-6.6s %.2f %.0f %.1f %.1f %.1f %.2f (%.3f, %.3f, %.3f)\n"
            , show_state(state)
            , (double) t
            , (double) rt
            , (double) yt
            , (double) temp_a
            , (double) temp_b
            , (double) ut
            , (double) g_p
            , (double) g_i
            , (double) g_d
        );

    ++i;
    delay_ms(100);
}

