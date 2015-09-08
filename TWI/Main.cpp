#include <AVR/Pins.h>
#include <AVR/Bits.h>
#include <AVR/Delay.h>
#include <AVR/Seg7.h>
#include <util/twi.h>

typedef pin_t<PD, 3> D0;
typedef pin_t<PC, 2> D1;
typedef pin_t<PB, 1> D2;
typedef pin_t<PD, 7> D3;

typedef pin_t<PD, 4> A;
typedef pin_t<PB, 2> B;
typedef pin_t<PD, 5> C;
typedef pin_t<PD, 1> D;
typedef pin_t<PD, 0> E;
typedef pin_t<PC, 3> F;
typedef pin_t<PD, 6> G;
typedef pin_t<PD, 2> H;

typedef bits_t<D0, D1, D2, D3> digit_t;
typedef bits_t<A, B, C, D, E, F, G, H> segment_t;
typedef seg7_t<digit_t, segment_t> seg7;

class twi_t
{
public:
	static void setup(uint32_t twi_freq = 100000)
	{
		TWSR = 0;								// set prescaler to 1
		TWBR = ((F_CPU / twi_freq) - 16) / 2;	// assuming prescaler 1
		TWCR = (1 << TWEN); 					// enable TWI
	}

	static bool write(uint8_t addr, volatile const uint8_t *src, uint8_t nw)
	{
		return write_read(addr, src, nw, 0, 0);
	}

	static bool read(uint8_t addr, volatile uint8_t *dst, uint8_t nr)
	{
		return write_read(addr, 0, 0, dst, nr);
	}

	static bool write_read(uint8_t addr, volatile const uint8_t *src, uint8_t nw, volatile uint8_t *dst, uint8_t nr)
	{
		if (s_busy)
			return false;

		s_busy = true;
		s_addr = addr;
		s_nw = nw;
		s_nr = nr;
		s_src = src;
		s_dst = dst;

		TWCR = TWINT_TWEN_TWIE | (1 << TWSTA); 						// start condition
	}

	static void wait_idle()
	{
		while (s_busy)
			delay_us(10);
	}

	static void isr()
	{
		switch (TWSR & 0xf8)
		{
		case TW_START:
			TWDR = s_addr | (s_nw ? 0 : 1);							// SLA+R/W
			TWCR = TWINT_TWEN_TWIE;									// send
			break;
		case TW_MT_SLA_ACK:											// from SLA+W
			TWDR = *s_src++;										// data
			TWCR = TWINT_TWEN_TWIE;									// send
			break;
		case TW_MT_DATA_ACK:										// from data write
			if (--s_nw)
			{
				TWDR = *s_src++;									// data
				TWCR = TWINT_TWEN_TWIE;								// send
			}
			else if (s_nr)
				TWCR = TWINT_TWEN_TWIE | (1 << TWSTA); 				// repeated start condition
			else
			{
				TWCR = (1 << TWINT) | (1 << TWSTO) | (1 << TWEN);	// stop condition
				s_busy = false;
			}
			break;
		case TW_REP_START:
			TWDR = s_addr | 1;										// SLA+R
			TWCR = TWINT_TWEN_TWIE;									// send
			break;
		case TW_MR_SLA_ACK:
			TWCR = TWINT_TWEN_TWIE;									// read
			break;
		case TW_MR_DATA_NACK:
			*s_dst++ = TWDR;
			if (--s_nr)
				TWCR = TWINT_TWEN_TWIE;								// read
			else
			{
				TWCR = (1 << TWINT) | (1 << TWSTO) | (1 << TWEN);	// stop condition
				s_busy = false;
			}
			break;
		default:
			TWCR = 0; // reset TWI subsystem
			s_busy = false;
		}
	}

private:
	static const uint8_t TWINT_TWEN_TWIE = (1 << TWINT) | (1 << TWEN) | (1 << TWIE);

	static volatile bool			s_busy;
	static volatile uint8_t			s_addr;
	static volatile uint8_t			s_nw;
	static volatile uint8_t			s_nr;
	static volatile const uint8_t	*s_src;
	static volatile uint8_t			*s_dst;
};

volatile bool twi_t::s_busy = false;
volatile uint8_t twi_t::s_addr;
volatile uint8_t twi_t::s_nw;
volatile uint8_t twi_t::s_nr;
volatile const uint8_t *twi_t::s_src;
volatile uint8_t *twi_t::s_dst;

ISR(TWI_vect)
{
	twi_t::isr();
}

void setup()
{
	seg7::setup();
	seg7::auto_refresh();

	seg7::write(0xbeef, 16);

	delay_ms(250);

	twi_t::setup(100000);
}

void loop()
{
//	static const uint8_t sla = 0x68;		// DS1307 address
	static const uint8_t sla = 0xA0;		// 24C32 address
	static uint8_t i = 0, j = 0;

	if (i < 250)
	{
		uint8_t buf[] = { 0, i, 255 - i, 1, 2, 3 };

		twi_t::write(0xA0, buf, sizeof(buf));
		delay_ms(10);
		twi_t::wait_idle();
		i += 4;
	}
	else
	{
		uint8_t adr[2] = { 0, j++ }, x;

		twi_t::write_read(sla, adr, sizeof(adr), &x, 1);
		twi_t::wait_idle();
		seg7::write(x);
		delay_ms(100);
	}
}

int main()
{
	setup();
	for (;;)
		loop();
}

