#include <AVR/Delay.h>
#include <AVR/Timer.h>

typedef timer_t<0> T;
typedef timer_t<1> U;
typedef timer_t<2> V;

void setup()
{
    const wg_mode mode = fast_pwm; // crashes non-deterinistically with pwm_phase_correct;

    T::setup<mode, top_0xff>();
    T::clock_select<1>();
    T::output_pin<channel_a>::setup();
    T::output_pin<channel_b>::setup();
    T::compare_output_mode<channel_a, clear_on_compare_match>();
    T::compare_output_mode<channel_b, clear_on_compare_match>();

    U::setup<mode, top_0xff>();
    U::clock_select<1>();
    U::output_pin<channel_a>::setup();
    U::output_pin<channel_b>::setup();
    U::compare_output_mode<channel_a, clear_on_compare_match>();
    U::compare_output_mode<channel_b, clear_on_compare_match>();

    V::setup<mode, top_0xff>();
    V::clock_select<1>();
    V::output_pin<channel_a>::setup();
    V::output_pin<channel_b>::setup();
    V::compare_output_mode<channel_a, clear_on_compare_match>();
    V::compare_output_mode<channel_b, clear_on_compare_match>();
}

static unsigned duty_cycle(unsigned& i, bool& dir)
{
    static const unsigned dcs[] = { 0, 0, 1, 1, 2, 2, 3, 4, 5, 7, 9, 12, 15
                                  , 19, 24 , 31, 39, 50, 63, 80, 101, 127, 160
                                  , 202, 254
                                  };

    if (i == 0 || (i + 1) == sizeof(dcs) / sizeof(*dcs))
        dir = !dir;

    unsigned dc = dcs[i];

    i += dir ? 1 : -1;
    return dc;
}


void loop()
{
    static unsigned t_idx_a = 0, t_idx_b = 0;
    static unsigned u_idx_a = 0, u_idx_b = 0;
    static unsigned v_idx_a = 0, v_idx_b = 0;
    static bool t_dir_a = false, t_dir_b = false;
    static bool u_dir_a = false, u_dir_b = false;
    static bool v_dir_a = false, v_dir_b = false;
    static unsigned i = 0;

    if (i % 13 == 0)
        T::output_compare_register<channel_a>() = duty_cycle(t_idx_a, t_dir_a);

    if (i % 17 == 0)
        T::output_compare_register<channel_b>() = duty_cycle(t_idx_b, t_dir_b);

    if (i % 23 == 0)
        U::output_compare_register<channel_a>() = duty_cycle(u_idx_a, u_dir_a);

    if (i % 29 == 0)
        U::output_compare_register<channel_b>() = duty_cycle(u_idx_b, u_dir_b);

    if (i % 31 == 0)
        V::output_compare_register<channel_a>() = duty_cycle(v_idx_a, v_dir_a);

    if (i % 37 == 0)
        V::output_compare_register<channel_b>() = duty_cycle(v_idx_b, v_dir_b);

    ++i;

	delay_ms(1);
}

int main()
{
	setup();
	for (;;)
		loop();
}

