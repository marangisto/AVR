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

class pid_reg_t
{
public:
    pid_reg_t(float dt, float kp, float ki, float kd)
        : m_dt(dt), m_kp(kp), m_ki(ki), m_kd(kd), m_int(0), m_err(0)
    {}

    float compute(float rt, float yt)
    {
        float err = rt - yt;
        float ut = m_kp * err;                          // proportional term

        m_int += err * m_dt;
        m_int = min(max<double>(m_int, -10.), 10.);     // clamping
        ut += m_ki * m_int;                             // integral term

        ut += m_kd * (err - m_err) / m_dt;              // derivative term
        m_err = err;

        return min(max<double>(ut, .0), 1.);
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
        PIN::write(m_x && m_x >= m_i++);
    }

    static void set(float x)    // [0..1] range
    {
        m_x = static_cast<uint8_t>(min(max(.0, 255. * x), 255.));
    }

private:
    static volatile uint8_t m_x;
    static uint8_t m_i;
};

template<class PIN> volatile uint8_t slow_pwm<PIN>::m_x = 0;
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

    float temp = 0.5 * (temp_a + temp_b);

    ut = pid_reg.compute(rt, temp);

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

void loop()
{
    static uint8_t i = 0;
    static bool running = true;

    if ((i & 0x5) == 0)
        ssr_b::toggle();

    float t = clock_time(clock_ticks);

    if (t < 10)
        rt = ambient;
    else if (t < 200)
        rt = 150;
    else if (t < 300)
        rt = ambient;
    else
        running = false;

    if (running)
    printf("%.2f %.0f %.1f %.1f %.2f\n"
        , (double) t
        , (double) rt
        , (double) temp_a
        , (double) temp_b
        , (double) ut
    );

    ++i;
    delay_ms(100);
}

