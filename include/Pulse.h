#pragma once

#include "Pins.h"
#include "Delay.h"

template<class PIN, bool PULSE_STATE>
uint32_t pulse_width(uint32_t timeout_us)
{
    const uint8_t x = PULSE_STATE ? PIN::mask : 0;
    uint32_t w = 0, i = 0, n = clocks_per_us * timeout_us / 16;

    while ((PIN::rpin() & PIN::mask) == x)
        if (++i > n)
            return 0;

    while ((PIN::rpin() & PIN::mask) != x)
        if (++i > n)
            return 0;

    while ((PIN::rpin() & PIN::mask) == x)
        if (++w, ++i > n)
            return 0;

    return (w * 17 + 16) / clocks_per_us;
}

