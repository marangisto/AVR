#pragma once

#include "Pins.h"

template<class PORT, int PIN>
class button_t
{
public:
    static inline bool read()
    {
        cli();
 
        bool b = m_pressed;

        m_pressed = false;
        sei();
        return b;
    }
 
    static inline void update()     // call from ISR
    {
        bool this_state = input::read();

        m_count = this_state != m_stable_state && this_state == m_last_state ? m_count + 1 : 0;
        if (m_count == m_stable_count)
            m_pressed = true;
        m_last_state = this_state;
    }

    static void setup(uint8_t stable_count = 8)
    {
        input::setup();
        m_stable_count = stable_count;
    }

private:
    typedef input_t<PORT, PIN> input;

    static bool m_stable_state;
    static bool m_last_state;
    static uint8_t m_stable_count;
    static uint8_t m_count;
    static bool m_pressed;
};

template<class PORT, int PIN> bool button_t<PORT, PIN>::m_stable_state = true;
template<class PORT, int PIN> bool button_t<PORT, PIN>::m_last_state = true;
template<class PORT, int PIN> uint8_t button_t<PORT, PIN>::m_stable_count = 8;
template<class PORT, int PIN> uint8_t button_t<PORT, PIN>::m_count = 0;
template<class PORT, int PIN> bool button_t<PORT, PIN>::m_pressed = false;

