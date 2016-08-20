#include <AVR/Main.h>
#include <AVR/ADC.h>
#include <AVR/UART.h>
#include <AVR/Timer.h>
#include <AVR/Delay.h>
#include <Arduino/Pins.h>
#include <stdio.h>
#include <string.h>
#include <stdlib.h>

template <class T> const T& max(const T& a, const T& b)
{
    return (a<b) ? b : a;
}

typedef output_t<PB, 0> STEP;
typedef output_t<PD, 7> DIR;
typedef output_t<PD, 6> ENABLE;

typedef timer_t<1> timer;

static const uint16_t timer_prescale = 64;

static inline uint16_t time_count(float t) { return static_cast<uint16_t>(t * F_CPU / timer_prescale); }

static const uint16_t steps_per_revolution = 200;
static const float max_revolutions_per_second = 2.7;
static const float min_step_time = 1. / (max_revolutions_per_second * steps_per_revolution);
static const uint16_t min_step_count = time_count(min_step_time);

static float accel_to_max_speed_in_steps(uint16_t n)
{
    float z = min_step_time / (sqrt(n + 1) - sqrt(n));

    return 2. / (z * z);
}

static volatile float s_accel = accel_to_max_speed_in_steps(steps_per_revolution * 10);

static inline float constant_acceleration_time(uint16_t l) { return sqrt(2. * l / s_accel); }

static volatile uint16_t s_nsteps = 5000;
static volatile uint16_t s_step = 0;
static volatile bool s_run = false;
static volatile float s_time = .0;

void isr()
{
    if (!s_run)
        return;

    STEP::set();
    //delay_us(1);    // minumum 1us

    float next_time = constant_acceleration_time(s_step + 1);

    timer::counter() = 65535 - max(min_step_count, time_count(next_time - s_time));

    s_run = ++s_step < s_nsteps;
    s_time = next_time;

    STEP::clear();
}

void run(uint16_t n, bool dir, float accel)
{
    printf("running %d steps %s...", n, dir ? "forward" : "reverse");

    DIR::write(dir);
    ENABLE::clear();

    s_accel = accel;
    s_nsteps = n;
    s_step = 0; 
    s_time = .0;
    s_run = true;

    while (s_run)
        ;

    ENABLE::set();
 
    printf("...done\n");
}

void setup()
{
    adc::setup<128>();

    timer::setup<normal_mode>();
    timer::clock_select<timer_prescale>();
    timer::isr(isr);
    timer::enable();

    STEP::setup();
    DIR::setup();
    ENABLE::setup();
    ENABLE::set();
    UART::setup<9600>();

    sei();
}

void loop()
{
    static bool dir = false;
    static uint16_t last_x = -1;
    uint16_t x = adc::read<5>();

    if (x != last_x)
    {
        printf("%d\n", x);
        last_x = x;
    }

    run(x + 200, dir = !dir, accel_to_max_speed_in_steps(x));
    delay_ms(10);
}

/*

void loop()
{
    static int i = 0;
    char buf[80] = "", *p;
    static uint16_t dt = 10;
    static int n = 0;

    printf("adc<5> = %d\n", adc::read<5>());
    printf("%04d> ", i++);

    if (!fgets(buf, sizeof(buf), stdin))
    {
        printf("reset\n");
        return;
    }

    if (p = strpbrk(buf, "\r\n"))
        *p = 0;

    printf("got '%s'\n", buf);

    switch (buf[0])
    {
    case 'T':
        dt = atoi(buf + 1);
        printf("dt = %d\n", dt);
        break;
    case 'R':
        if (buf[1])
            n = atoi(buf + 1);
        printf("running %d\n", n);
        run(abs(n), n > 0, dt);
        break;
    case '\0':
        printf("repeat %d\n", n);
        run(abs(n), n > 0, dt);
        break;
    }
}

 
 */
