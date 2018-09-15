#define NO_TIMER_VECTORS 1
#include <AVR/Main.h>
#include <AVR/Delay.h>
#include <AVR/Timer.h>
#include <AVR/ADC.h>
#include <AVR/Pins.h>

template <class T> const T& max(const T& a, const T& b) { return (a<b) ? b : a; }
template <class T> const T& min(const T& a, const T& b) { return (a<b) ? a : b; }

typedef output_t<PD, 2> trig;

typedef output_t<PD, 3> led_0;
typedef output_t<PD, 4> led_1;
typedef output_t<PD, 5> led_2;
typedef output_t<PD, 6> led_3;
typedef output_t<PD, 7> led_trig;

typedef outputs_t<led_0, led_1, led_2, led_3> led_0123;

typedef input_t<PB, 0, enable_pullup> clk;

typedef timer_t<1> pwm;
typedef timer_t<2> aux;

static volatile uint8_t display = 0;

ISR(TIMER2_OVF_vect)
{
    static uint8_t i = 0;

    led_0123::write(i++ < 10 ? display : 0);
}

void setup()
{
    led_0::setup();
    led_1::setup();
    led_2::setup();
    led_3::setup();
    led_trig::setup();
    trig::setup();
    clk::setup();
    adc::setup<128>();

    const wg_mode mode = pwm_phase_correct;
    const int prescale = 1;

    pwm::setup<mode, top_0xff>();
    pwm::clock_select<prescale>();
    pwm::output_pin<channel_a>::setup();
    pwm::compare_output_mode<channel_a, clear_on_compare_match>();

    aux::setup<normal_mode>();
    aux::clock_select<1>();
    aux::enable();

    /*
    PCMSK0 |= _BV(PCINT0);
    PCICR |= _BV(PCIE0);
    */
    sei();
}

void loop()
{
    static uint8_t i = 0;

    pwm::output_compare_register<channel_a>() = 255 - (i % 12);
    trig::set();
    led_trig::set();
    delay_ms(10);
    trig::clear();
    led_trig::clear();

    display = i;
    delay_ms(100);

    i += 1;
}

