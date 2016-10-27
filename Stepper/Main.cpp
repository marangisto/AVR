#include <AVR/Main.h>
//#include <AVR/ADC.h>
#include <AVR/UART.h>
#include <AVR/Timer.h>
#include <AVR/Delay.h>
#include <Arduino/Pins.h>
#include <stdio.h>
#include <string.h>
#include <stdlib.h>

template <class T> const T& max(const T& a, const T& b) { return (a<b) ? b : a; }
template <class T> const T& min(const T& a, const T& b) { return (a<b) ? a : b; }
template <class T> const T sqr(T a) { return a * a; }

typedef output_t<PD, 2> STEP;
typedef output_t<PD, 5> DIR;
typedef output_t<PB, 0> ENABLE;

//typedef output_t<PB, 1> MS1;
//typedef output_t<PB, 2> MS2;
//typedef output_t<PB, 3> MS3;

//typedef outputs_t<MS1, MS2, MS3> MS;

template<uint16_t STEPS_PER_REVOLUTION, uint16_t MAX_RPM>
struct stepper_traits_t
{
    static const uint16_t timer_prescale = 64;
    static uint16_t time_count(float t) { return static_cast<uint16_t>(t * F_CPU / timer_prescale); }
    static float min_step_time() { return 1. / (STEPS_PER_REVOLUTION * MAX_RPM / 60.); }
    static uint16_t min_step_count() { return time_count(min_step_time()); }
    static float accel_to_max_speed_in_steps(uint16_t n) { return 2. / sqr(min_step_time() / (sqrt(n + 1) - sqrt(n))); }
    static inline float constant_acceleration_time(float accel, uint16_t l) { return sqrt(2. * l / accel); }
    static uint16_t steps_to_max_speed(float accel) { return static_cast<uint16_t>(sqr(1. / min_step_time()) / (2. * accel)); }
};

typedef stepper_traits_t<200, 162> stepper_traits; // 17HS1003
typedef timer_t<1> timer;

enum stepper_state_t { ss_stop, ss_accel, ss_cruise, ss_decel };

static volatile stepper_state_t s_state = ss_stop;
static volatile float s_accel = stepper_traits::accel_to_max_speed_in_steps(200);
static volatile uint16_t s_nsteps = 0;
static volatile uint16_t s_nramp = 0;
static volatile uint16_t s_step = 0;
static volatile uint8_t s_microsteps = 1;

static uint8_t ms_bits(uint8_t ms)
{
    switch (ms)
    {
        case 1: return 0x0;
        case 2: return 0x1;
        case 4: return 0x2;
        case 8: return 0x3;
        case 16: return 0x7;
        default: return 0x0;
    }
}

void isr()
{
    static uint16_t step_count;
    static float step_time, l_t;

    if (s_state == ss_stop)
        return;

    STEP::set();

    switch (s_state)
    {
    case ss_accel:
        if (s_step == 0)
            l_t = 0;
        step_time = stepper_traits::constant_acceleration_time(s_accel, s_step + 1) - l_t;
        step_count = stepper_traits::time_count(step_time);
        l_t += step_time;
        break;
    case ss_cruise:
        break;
    case ss_decel:
        if (s_step + s_nramp <= s_nsteps) // only true once
            l_t = stepper_traits::constant_acceleration_time(s_accel, s_nsteps - s_step - 1);
        step_time = l_t - stepper_traits::constant_acceleration_time(s_accel, s_nsteps - s_step - 2);
        step_count = stepper_traits::time_count(step_time);
        l_t -= step_time;
        break;
    case ss_stop:
        return;     // never reached
    }

    delay_us(1);    // minumum 1us
    STEP::clear();

    timer::counter() = 65535 - step_count;

    if (++s_step < s_nramp)
        s_state = ss_accel;
    else if (s_step < s_nsteps - s_nramp)
        s_state = ss_cruise;
    else if (s_step < s_nsteps)
        s_state = ss_decel;
    else
        s_state = ss_stop;
}

void run(uint16_t n, bool dir, float accel)
{
    printf("steps to max-speed = %d\n", stepper_traits::steps_to_max_speed(accel));

    printf("running %d steps %s...", n, dir ? "forward" : "reverse");

    DIR::write(dir);
    //MS::write(ms_bits(s_microsteps)); // HARDWARE SET

    s_accel = accel;
    s_nsteps = n;
    s_nramp = min(n / 2, stepper_traits::steps_to_max_speed(accel));
    s_step = 0;
    s_state = ss_accel;

    while (s_state != ss_stop)
        ;

    printf("...done\n");
}

void setup()
{
    //adc::setup<128>();

    timer::setup<normal_mode>();
    timer::clock_select<stepper_traits::timer_prescale>();
    timer::isr(isr);
    timer::enable();

    //MS::setup();
    STEP::setup();
    DIR::setup();
    ENABLE::setup();
    ENABLE::set();
    UART::setup<9600>();

    sei();
}

void loop()
{
    char buf[80] = "", *p;
    
    if (!fgets(buf, sizeof(buf), stdin))
    {
        printf("reset\n");
        return;
    }

    if ((p = strpbrk(buf, "\r\n")))
        *p = 0;

    printf("got '%s'\n", buf);

    static int16_t res[16];
    static uint8_t nres = 0;
    static uint16_t acc_steps = 50;    // acceleration steps to max speed

    switch (buf[0])
    {
    case 'E':
        ENABLE::clear();
        return;
    case 'D':
        ENABLE::set();
        return;
    case 'S':
        acc_steps = atoi(buf + 1);
        printf("acceleration steps to max speed = %d\n", acc_steps);
        return;
    case 'M':
        s_microsteps = atoi(buf + 1);
        printf("microsteps = %d\n", s_microsteps);
        return;
    case 'G':
        nres = 0;
        for (char *p = strtok(buf + 1, " "); p; p = strtok(0, " "))
            if (nres < sizeof(res) / sizeof(*res))
                res[nres++] = atoi(p);
        break;
    case '\0':
        // repeat run
        break;
    default:
        printf("unrecognized command: '%s'\n", buf);
        return;
    }

    printf("%d\n", nres);

    /*
    static uint16_t last_x = -1;
    uint16_t x = adc::read<5>();

    if (x != last_x)
    {
        printf("%d\n", x);
        last_x = x;
    }
    */

    for (uint8_t i = 0; i < nres; ++i)
        run(abs(res[i]) * s_microsteps, res[i] > 0, stepper_traits::accel_to_max_speed_in_steps(acc_steps));
    delay_ms(10);
}

