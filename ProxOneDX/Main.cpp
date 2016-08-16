#include <AVR/Main.h>
#include <AVR/Pulse.h>

typedef pin_t<PB, 0> FOCUS;
typedef pin_t<PB, 1> EXPOSE;
typedef pin_t<PB, 3> TRIG;
typedef pin_t<PB, 4> ECHO;

void shoot()
{
    set<FOCUS>();
    delay_ms(10);
    set<EXPOSE>();
    delay_ms(1000);
    clear<EXPOSE>();
    clear<FOCUS>();
}


uint32_t dist_pulse()
{
    set<TRIG>();
    delay_us(10);
    clear<TRIG>();
    return pulse_width<ECHO, true>(10000);
}


template<class T> void swap(T& x, T& y) { T z = x; x = y; y = z; }

float distance()
{
    static const uint8_t n = 5;
    uint32_t ws[n];

    for (uint8_t i = 0; i < n; ++i)
    {
        ws[i] = dist_pulse();
        delay_ms(10);
    }

    bool more = true;

    while (more)
    {
        more = false;
        for (uint8_t i = 0; i < n - 1; ++i)
            if (ws[i] > ws[i + 1])
            {
                swap(ws[i], ws[i + 1]);
                more = true;
            }
    }

    return (ws[1] + ws[2] + ws[3]) / (3 * 2 * 29.4);    // 340m/s -> 29.4us/cm
}

void setup()
{
    digital_out<FOCUS>();
    digital_out<EXPOSE>();
    digital_out<TRIG>();
    digital_in<ECHO>();
}

void loop()
{
    float d = distance();

    if (d > 95 && d < 105)
    {
        shoot();
        delay_ms(2000);
    }

    delay_ms(20);
}

