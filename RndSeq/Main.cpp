#define NO_TIMER_VECTORS 1
#include <AVR/Main.h>
#include <AVR/Delay.h>
#include <AVR/Timer.h>
#include <AVR/ADC.h>
#include <AVR/Pins.h>
#include <stdlib.h>

template <class T> const T& max(const T& a, const T& b) { return (a<b) ? b : a; }
template <class T> const T& min(const T& a, const T& b) { return (a<b) ? a : b; }

typedef output_t<PD, 2> trig;

typedef output_t<PD, 3> led_0;
typedef output_t<PD, 4> led_1;
typedef output_t<PD, 5> led_2;
typedef output_t<PD, 6> led_3;
typedef output_t<PD, 7> led_trig;

typedef outputs_t<led_0, led_1, led_2, led_3> led_0123;

typedef input_t<PB, 0> clk;

static const int adc_pot = 0;
static const int adc_cv = 1;

typedef timer_t<1> pwm;
typedef timer_t<2> aux;

static volatile uint8_t g_display = 0;
static volatile uint16_t g_randomness = 0;

static const float min_V = 0.197;
static const float max_V = 4.721;

static const uint8_t n_notes = (max_V - min_V) * 12;    // 

static uint16_t g_notes[n_notes];


static void initialize_notes()
{
    int steps = 0x1ff;              // must agree with pwm timer top count
    float range = max_V - min_V;
    float dV = range / steps;
    float note = 1. / 12.;

    for (size_t i = 0; i < n_notes; ++i)
        g_notes[i] = i * note / dV;
}

static volatile uint16_t g_sequence[256] = { 0, 2, 4, 3, 6, 7, 12, 5 };

static volatile uint8_t g_steps = 8;
static volatile uint8_t g_range = 24;

static void step()
{
    static uint8_t i = 0;

    if ((rand() & 0x3ff) < g_randomness)
    {
        g_sequence[i] = rand() % g_range;
    }

    uint8_t note = g_sequence[i];
    uint16_t duty = g_notes[min(note, n_notes)];

    pwm::output_compare_register<channel_a>() = 0x1ff - duty;   // inverted output
    trig::clear();
    delay_ms(10);
    trig::set();
    g_display = i;
    i = ++i < g_steps ? i : 0;
}

ISR(TIMER2_OVF_vect)
{
    static uint8_t i = 0;

    led_0123::write(i++ < 15 ? g_display : 0);
}

ISR(PCINT0_vect)
{
    static bool last_clk = false;

    bool c = clk::read();

    if (c != last_clk)
    {
        led_trig::write(c);
        if (c)
            step();
        last_clk = c;
    }
}

void setup()
{
    initialize_notes();

    led_0::setup();
    led_1::setup();
    led_2::setup();
    led_3::setup();
    led_trig::setup();
    trig::setup();
    clk::setup();
    adc::setup<128>();

    trig::set();      // because output buffer is inverted

    pwm::setup<fast_pwm, top_0x1ff>();
    pwm::clock_select<1>();
    pwm::output_pin<channel_a>::setup();
    pwm::compare_output_mode<channel_a, clear_on_compare_match>();

    aux::setup<normal_mode>();
    aux::clock_select<1>();
    aux::enable();

    PCMSK0 |= _BV(PCINT0);
    PCICR |= _BV(PCIE0);

    sei();
    srand(1);
}

void loop()
{
//    pwm::output_compare_register<channel_a>() = 0x1ff;   // inverted output
    g_randomness = adc::read<adc_pot>();
    delay_ms(10);
}

