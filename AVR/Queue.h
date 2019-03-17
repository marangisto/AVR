#pragma once

#include "Timer.h" // for critical section

template<class T, uint8_t IDENT, uint8_t BUFSIZE>
class queue_t
{
public:
    static bool get(T& x)
    {
        critical_section_t cs;          // enter critical section

        if (m_ridx == m_widx)
            return false;
        x = m_buf[m_ridx++ & mask];
        if (m_ridx == m_widx)           // rebase indices
            m_ridx = m_widx = 0;
        return true;
    }

    inline static void put(T x)         // use in interrupt only
    {
        static_assert((BUFSIZE != 0) && !(BUFSIZE & (BUFSIZE - 1)), "buffer size must be a power of 2");

        if (m_widx < m_ridx + BUFSIZE)        // avoid overflow
            m_buf[m_widx++ & mask] = x;
    }

private:
    static const uint8_t mask = BUFSIZE - 1;
    static T m_buf[BUFSIZE];
    static uint8_t m_widx, m_ridx;
};

template<class T, uint8_t IDENT, uint8_t BUFSIZE> T queue_t<T, IDENT, BUFSIZE>::m_buf[BUFSIZE];
template<class T, uint8_t IDENT, uint8_t BUFSIZE> uint8_t queue_t<T, IDENT, BUFSIZE>::m_widx = 0;
template<class T, uint8_t IDENT, uint8_t BUFSIZE> uint8_t queue_t<T, IDENT, BUFSIZE>::m_ridx = 0;

