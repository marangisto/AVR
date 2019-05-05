#ifndef BUTTONS_H
#define BUTTONS_H

#include "ADC.h"

template<int CH>
class buttons_t
{
public:
    static const uint8_t mask = 0x0f;
    static const uint8_t fast = 0x10;

    static void setup()
    {
        adc::setup<128>();
    }

    static uint8_t read()
    {

        uint8_t x = raw_read();

        if (x == 0)
        {
            count = 0;

            if (last_y != 0)
            {
                uint8_t y = last_y;

                last_y = 0;
                return y;
            }
        }
        else if (x == last_x)
        {
            if (++count > 10)
                last_y = x;
            if (count > 5000 && (count & 0x0f) == 0)
                return x | fast;
            if (count > 2000 && (count & 0xff) == 0)
                return x | fast;
            if (count > 500 && (count & 0xff) == 0)
                return x;
        }
        else
        {
            count = 0;
            last_x = x;
        }

        return 0;
    }

private:
    static uint8_t raw_read()
    {
        static uint16_t limits[] = { 940, 751, 533, 302, 92 };
        static uint8_t n_limits = sizeof(limits) / sizeof(*limits);
        uint16_t x = adc::read<CH>();

        for (uint8_t i = 0; i < n_limits; ++i)
            if (x > limits[i])
                return i;

        return n_limits;
    }

    static uint8_t last_x;
    static uint8_t last_y;
    static uint16_t count;
};

template<int CH> uint8_t buttons_t<CH>::last_x = 0;
template<int CH> uint8_t buttons_t<CH>::last_y = 0;
template<int CH> uint16_t buttons_t<CH>::count = 0;

#endif // BUTTONS_H
