#include <AVR/Delay.h>
#include <AVR/Timer.h>

typedef output_t<PB, 0> S0;
typedef output_t<PD, 7> S1;
typedef input_t<PD, 0, enable_pullup> BT0;
typedef input_t<PD, 1, enable_pullup> BT1;

template<uint8_t TIMER, uint8_t MILLISECS, uint8_t BUFSIZE>
class debouncer_t
{
public:
    typedef timer_t<TIMER> timer;

    static void setup()
    {
        // set timer for ~1kHz isr
        timer::template setup<normal_mode>();
        timer::template clock_select<64>();
        timer::isr(isr);
        timer::enable();
    }
 
    static bool get(bool& x)
    {
        critical_section_t cs;          // enter critical section

        if (m_ridx == m_widx)
            return false;
        x = m_buf[m_ridx++ & mask];
        if (m_ridx == m_widx)           // rebase indices
            m_ridx = m_widx = 0;
        return true;
    }

private:
    static void isr()
    {
        static uint8_t count = 0;
        static bool last_state = true;
        bool this_state = BT0::read();

        count = this_state != stable_state && this_state == last_state ? count + 1 : 0;
        if (count == stable_count)
            put(stable_state = this_state);
        last_state = this_state;
    }

    inline static void put(bool x)
    {
        static_assert((BUFSIZE != 0) && !(BUFSIZE & (BUFSIZE - 1)), "buffer size must be a power of 2");

        if (m_widx < m_ridx + BUFSIZE)        // avoid overflow
            m_buf[m_widx++ & mask] = x;
    }
 
    // debouncing state
 
    static const uint8_t stable_count;
    static bool stable_state;

    // output buffer

    static const uint8_t mask = BUFSIZE - 1;
    static bool m_buf[BUFSIZE];
    static uint8_t m_widx, m_ridx;
};

template<uint8_t TIMER, uint8_t MILLISECS, uint8_t BUFSIZE> const uint8_t debouncer_t<TIMER, MILLISECS, BUFSIZE>::stable_count = MILLISECS;
template<uint8_t TIMER, uint8_t MILLISECS, uint8_t BUFSIZE> bool debouncer_t<TIMER, MILLISECS, BUFSIZE>::stable_state = true;
template<uint8_t TIMER, uint8_t MILLISECS, uint8_t BUFSIZE> bool debouncer_t<TIMER, MILLISECS, BUFSIZE>::m_buf[BUFSIZE];
template<uint8_t TIMER, uint8_t MILLISECS, uint8_t BUFSIZE> uint8_t debouncer_t<TIMER, MILLISECS, BUFSIZE>::m_widx = 0;
template<uint8_t TIMER, uint8_t MILLISECS, uint8_t BUFSIZE> uint8_t debouncer_t<TIMER, MILLISECS, BUFSIZE>::m_ridx = 0;

typedef debouncer_t<2, 20, 8> debouncer;

void setup()
{
    S0::setup();
    S1::setup();
    BT0::setup();
    BT1::setup();
    debouncer::setup();
    sei();      // need explcit interrup-enable at end of setup
}

void loop()
{
    bool b1 = BT1::read();
    
    S1::write(b1);

    bool b0;

    if (debouncer::get(b0) && b0)
        S0::toggle();
    delay_ms(250);
    //delay_ms(1);
}

int main()
{
    setup();
    for (;;)
        loop();
}

