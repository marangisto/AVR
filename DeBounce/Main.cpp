#include <AVR/Delay.h>
#include <AVR/Timer.h>

typedef output_t<PB, 0> IC;
typedef output_t<PD, 7> CK;
typedef input_t<PD, 0, enable_pullup> BT0;

typedef timer_t<2> T;


template<int N>
class buffer_t
{
public:
    static void put(bool x)
    {
        static_assert((N != 0) && !(N & (N - 1)), "buffer size must be a power of 2");

        if (m_widx < m_ridx + N)        // avoid overflow
            m_buf[m_widx++ & mask] = x;
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
    static const uint8_t mask = N - 1;

    static bool m_buf[N];
    static uint8_t m_widx, m_ridx;
};


template<int N> bool buffer_t<N>::m_buf[N];
template<int N> uint8_t buffer_t<N>::m_widx = 0;
template<int N> uint8_t buffer_t<N>::m_ridx = 0;


template<int TNO, int N>
class debouncer_t
{
public:
    typedef timer_t<TNO> timer;

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
        return buffer_t<N>::get(x);
    }

private:
    static void isr()
    {
        static uint8_t count = 0;
        static bool last_state = true;
        bool this_state = BT0::read();

        count = this_state != stable_state && this_state == last_state ? count + 1 : 0;
        if (count == stable_count)
            buffer_t<N>::put(stable_state = this_state);
        last_state = this_state;
    }

    static const uint8_t stable_count;
    static bool stable_state;
};

template<int TNO, int N> const uint8_t debouncer_t<TNO, N>::stable_count = 20;
template<int TNO, int N> bool debouncer_t<TNO, N>::stable_state = true;


typedef debouncer_t<2, 8> debouncer;

void setup()
{
	IC::setup();
	CK::setup();
    BT0::setup();
    debouncer::setup();
	sei();      // need explcit interrup-enable at end of setup
}


void loop()
{
    bool b = BT0::read();
    
    CK::write(b);

    bool b2;

    if (debouncer::get(b2) && b2)
        IC::toggle();
    delay_ms(250);
	//delay_ms(1);
}

int main()
{
	setup();
	for (;;)
		loop();
}

