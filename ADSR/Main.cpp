#include <AVR/Main.h>
#include <AVR/Delay.h>
#include <AVR/Timer.h>
#include <Arduino/Pins.h>

typedef D13 LED;
typedef timer_t<0> T;

void setup()
{
    LED::setup();
    const wg_mode mode = pwm_phase_correct;
    //const wg_mode mode = fast_pwm;
    const int prescale = 1;

    T::setup<mode, top_0xff>();
    T::clock_select<prescale>();
    T::output_pin<channel_a>::setup();
    T::output_pin<channel_b>::setup();
    T::compare_output_mode<channel_a, clear_on_compare_match>();
}

void loop()
{
    static const int dcs[] = { 255, 0};
    static unsigned i = 0;
    static bool dir = false;            // scan direction

    T::output_compare_register<channel_a>() = dcs[i];

    if (i == 0 || (i + 1) == sizeof(dcs) / sizeof(*dcs))
        dir = !dir;
    
    i += dir ? 1 : -1;

    LED::toggle();

    delay_us(500);
}

