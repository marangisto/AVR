#include <AVR/Pins.h>
#include <AVR/Delay.h>
#include <AVR/Timer.h>

typedef output_t<PB, 1> A;
typedef output_t<PB, 2> B;
typedef timer_t<1> T;

void setup()
{
	A::setup();
	B::setup();
    T::setup<fast_pwm, top_0x3ff>();
    T::clock_select<1>();
    T::compare_output_mode<channel_a, clear_on_compare_match>();
    T::compare_output_mode<channel_b, clear_on_compare_match>();
}

static unsigned duty_cycle(unsigned& i, bool& dir)
{
    static const int dcs[] = { 0, 0, 1, 1, 2, 2, 3, 4, 5, 7, 9, 12, 15
                             , 19, 24 , 31, 39, 50, 63, 80, 101, 127, 160
                             , 202, 255 , 322, 405, 511, 644, 812, 1022
                             };

    if (i == 0 || (i + 1) == sizeof(dcs) / sizeof(*dcs))
        dir = !dir;

    int dc = dcs[i];

    i += dir ? 1 : -1;
    return dc;
}


void loop()
{
    //  map ((\x -> x - 1) . round . (2**) . (/3)) [0..30] -- adjust final element to be one less than TOP
    static const int dcs[] = { 0, 0, 1, 1, 2, 2, 3, 4, 5, 7, 9, 12, 15
                             , 19, 24 , 31, 39, 50, 63, 80, 101, 127, 160
                             , 202, 255 , 322, 405, 511, 644, 812, 1022
                             };
    static unsigned i = 0, j = 0, k = 0;
    static bool dir_a = false, dir_b = false;            // scan direction

    if (i % 21 == 0)
        T::output_compare_register<channel_a>() = duty_cycle(j, dir_a);

    if (i % 17 == 0)
        T::output_compare_register<channel_b>() = duty_cycle(k, dir_b);

    ++i;

	delay_ms(1);
}

int main()
{
	setup();
	for (;;)
		loop();
}

