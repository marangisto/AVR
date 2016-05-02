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


static const uint8_t stable_count = 20;
static bool stable_state = true;

static void isr()
{
    static uint8_t count = 0;
    static bool last_state = true;

    bool this_state = BT0::read();

    count = this_state != stable_state && this_state == last_state ? count + 1 : 0;

    if (count == stable_count)
        buffer_t::put(stable_state = this_state);

    last_state = this_state;

    //IC::write(stable_state);
}

void setup()
{
	IC::setup();
	CK::setup();
    BT0::setup();

    // set timer for ~1kHz isr
    T::setup<normal_mode>();
    T::clock_select<64>();
	T::isr(isr);
	T::enable();
	sei();
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

