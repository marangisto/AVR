#pragma once

enum chan_t { chan_a = 0x0, chan_b = 0x1 << 15 };
enum gain_t { gain_x1 = 0x1 << 13, gain_x2 = 0x0 };

struct mcp48x2_t
{
    static const uint16_t _shutdown = 0x1 << 12;

    template<chan_t CH, gain_t GAIN = gain_x1>
    static inline uint16_t encode(uint8_t x)
    {
        return CH | GAIN | _shutdown | (x << 4);
    }

    template<chan_t CH, gain_t GAIN = gain_x1>
    static inline uint16_t encode(uint16_t x)
    {
        return CH | GAIN | _shutdown | (x & 0x0fff);
    }
};

