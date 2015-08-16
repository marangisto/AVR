#ifndef PULSE_H
#define PULSE_H

#include "Pins.h"
#include "Delay.h"

template<class PIN, bool PULSE_STATE>
uint32_t pulse_width(uint32_t timeout_us)
{
    uint32_t w = 0, i = 0, n = clocks_per_us * timeout_us / 16;

    while (read<PIN>() == PULSE_STATE)
        if (++i > n)
            return 0;

    while (read<PIN>() != PULSE_STATE)
        if (++i > n)
            return 0;

    while (read<PIN>() == PULSE_STATE)
        if (++w, ++i > n)
            return 0;

    return (w * 10 + 16) / clocks_per_us;
}

#endif PULSE_H

