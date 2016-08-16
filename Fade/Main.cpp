#include <AVR/Delay.h>
#include <AVR/Timer.h>

typedef timer_t<1> T;

void setup()
{
    T::setup<fast_pwm, top_0x3ff>();
    T::clock_select<1>();
    T::output_pin<channel_a>::setup();
    T::compare_output_mode<channel_a, clear_on_compare_match>();
}

void loop()
{
    //  map ((\x -> x - 1) . round . (2**) . (/3)) [0..30] -- adjust final element to be one less than TOP
    static const int dcs[] = { 0, 0, 1, 1, 2, 2, 3, 4, 5, 7, 9, 12, 15
                             , 19, 24 , 31, 39, 50, 63, 80, 101, 127, 160
                             , 202, 255 , 322, 405, 511, 644, 812, 1022
                             };
    static unsigned i = 0;
    static bool dir = false;            // scan direction

    T::output_compare_register<channel_a>() = dcs[i];

    if (i == 0 || (i + 1) == sizeof(dcs) / sizeof(*dcs))
        dir = !dir;
    
    i += dir ? 1 : -1;

    delay_ms(25);
}

int main()
{
    setup();
    for (;;)
        loop();
}

