#include <AVR/Delay.h>
#include <AVR/Timer.h>

typedef timer_t<0> T;

void setup()
{
    T::setup<pwm_phase_correct, top_0xff>();
    T::clock_select<1>();
    T::output_pin<channel_a>::setup();
    T::output_pin<channel_b>::setup();
    T::compare_output_mode<channel_a, clear_on_compare_match>();
    T::compare_output_mode<channel_b, clear_on_compare_match>();
}

static unsigned duty_cycle(unsigned& i, bool& dir)
{
    static const int dcs[] = { 0, 0, 1, 1, 2, 2, 3, 4, 5, 7, 9, 12, 15
                             , 19, 24 , 31, 39, 50, 63, 80, 101, 127, 160
                             , 202, 254
                             };

    if (i == 0 || (i + 1) == sizeof(dcs) / sizeof(*dcs))
        dir = !dir;

    int dc = dcs[i];

    i += dir ? 1 : -1;
    return dc;
}


void loop()
{
    static unsigned i = 0, idx_a = 0, idx_b = 0;
    static bool dir_a = false, dir_b = false;            // scan direction

    if (i % 21 == 0)
        T::output_compare_register<channel_a>() = duty_cycle(idx_a, dir_a);

    if (i % 17 == 0)
        T::output_compare_register<channel_b>() = duty_cycle(idx_b, dir_b);

    ++i;

	delay_ms(1);
}

int main()
{
	setup();
	for (;;)
		loop();
}

