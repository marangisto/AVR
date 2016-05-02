#include <AVR/Delay.h>
#include <AVR/Timer.h>

typedef output_t<PB, 0> IC;
typedef output_t<PD, 7> CK;
typedef input_t<PD, 0, enable_pullup> BT0;

typedef timer_t<2> T;

class buffer_t
{
public:
    static void put(bool x)
    {
        if (m_widx < m_ridx + 8)        // avoid overflow
            m_buf[m_widx++ & 0x7] = x;
    }

    static bool get(bool& x)
    {
        critical_section_t cs;          // enter critical section

        if (m_ridx == m_widx)
            return false;
        x = m_buf[m_ridx++ & 0x7];
        if (m_ridx == m_widx)           // rebase indices
            m_ridx = m_widx = 0;
        return true;
    }

private:
    static bool m_buf[8];
    static uint8_t m_widx, m_ridx;
};


bool buffer_t::m_buf[8];
uint8_t buffer_t::m_widx = 0;
uint8_t buffer_t::m_ridx = 0;


template<int TNO>
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
    
private:
    static void isr()
    {
        static uint8_t count = 0;
        static bool last_state = true;
        bool this_state = BT0::read();

        count = this_state != stable_state && this_state == last_state ? count + 1 : 0;
        if (count == stable_count)
            buffer_t::put(stable_state = this_state);
        last_state = this_state;
    }

    static const uint8_t stable_count;
    static bool stable_state;
};

template<int TNO> const uint8_t debouncer_t<TNO>::stable_count = 20;
template<int TNO> bool debouncer_t<TNO>::stable_state = true;


typedef debouncer_t<2> debouncer;

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

    if (buffer_t::get(b2) && b2)
        IC::toggle();
	//delay_ms(1);
}

int main()
{
	setup();
	for (;;)
		loop();
}

