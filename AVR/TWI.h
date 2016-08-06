#ifndef TWI_H
#define TWI_H

#include <AVR/Pins.h>
#include <AVR/Delay.h>
#include <util/twi.h>
#include <avr/interrupt.h>

class twi_t
{
public:
	static void setup(uint32_t twi_freq = 100000)
	{
		TWSR = 0;								// set prescaler to 1
		TWBR = ((F_CPU / twi_freq) - 16) / 2;	// assuming prescaler 1
		TWCR = _BV(TWEN); 						// enable TWI
	}

	static uint8_t write(uint8_t addr, volatile const uint8_t *src, uint8_t nw)
	{
		write_read(addr, src, nw, 0, 0);
        return wait_idle();
	}

	static uint8_t read(uint8_t addr, volatile uint8_t *dst, uint8_t nr)
	{
		write_read(addr, 0, 0, dst, nr);
        return wait_idle();
	}

	static void write_read(uint8_t addr, volatile const uint8_t *src, uint8_t nw, volatile uint8_t *dst, uint8_t nr)
	{
		if (s_busy)
        {
            s_err = 255;                                            // busy
            return;
        }

        s_err = 0;
		s_busy = true;
		s_addr = addr;
		s_nw = nw;
		s_nr = nr;
		s_src = src;
		s_dst = dst;

		TWCR = TWINT_TWEN_TWIE | _BV(TWSTA); 						// start condition
	}

	static inline uint8_t wait_idle()
	{
		while (s_busy)
			delay_us(10);
        return s_err;
	}

	static void isr()
	{
		switch (TWSR & 0xf8)
		{
		case TW_START: ;
		case TW_REP_START:
			TWDR = (s_addr << 1) | (s_nw ? 0 : 1);					// SLA+R/W
			TWCR = TWINT_TWEN_TWIE;									// send
			return;
		case TW_MT_SLA_ACK:											// from SLA+W
			TWDR = *s_src++;										// data
			TWCR = TWINT_TWEN_TWIE;									// send
			return;
		case TW_MT_DATA_ACK:										// from data write
			if (--s_nw)
			{
				TWDR = *s_src++;									// data
				TWCR = TWINT_TWEN_TWIE;								// send
			}
			else if (s_nr)
				TWCR = TWINT_TWEN_TWIE | _BV(TWSTA); 				// repeated start condition
			else
				break;												// stop
			return;
		case TW_MR_DATA_ACK:
			*s_dst++ = TWDR; 										// fall through, same code as SLA_ACK
		case TW_MR_SLA_ACK:
			TWCR = TWINT_TWEN_TWIE |  (--s_nr ? _BV(TWEA) : 0);		// ack if more to read
			return;
		case TW_MR_DATA_NACK:
			*s_dst++ = TWDR;
			break;													// stop
		default:
            s_err = TWSR & 0xf8;								    // stop + error
		}

		TWCR = _BV(TWINT) | _BV(TWSTO) | _BV(TWEN);					// stop condition
		s_busy = false;
	}

private:
	static const uint8_t TWINT_TWEN_TWIE = _BV(TWINT) | _BV(TWEN) | _BV(TWIE);

	static volatile bool			s_busy;
	static volatile uint8_t			s_addr;
	static volatile uint8_t			s_nw;
	static volatile uint8_t			s_nr;
    static volatile uint8_t         s_err;
	static volatile const uint8_t	*s_src;
	static volatile uint8_t			*s_dst;
};

volatile bool twi_t::s_busy = false;
volatile uint8_t twi_t::s_addr;
volatile uint8_t twi_t::s_nw;
volatile uint8_t twi_t::s_nr;
volatile uint8_t twi_t::s_err;
volatile const uint8_t *twi_t::s_src;
volatile uint8_t *twi_t::s_dst;

ISR(TWI_vect)
{
	twi_t::isr();
}

#endif TWI_H

