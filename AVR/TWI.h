#pragma once

#include <AVR/Pins.h>
#include <AVR/Delay.h>
#include <util/twi.h>
#include <avr/interrupt.h>

class twi_t
{
public:
    static inline void setup(uint32_t twi_freq = 100000)
    {
        TWSR = 0;                                                   // set prescaler to 1
        TWBR = ((F_CPU / twi_freq) - 16) / 2;                       // assuming prescaler 1
        TWCR = _BV(TWEN);                                           // enable TWI
    }

    static inline uint8_t write(uint8_t addr, volatile const uint8_t *src, uint8_t nw)
    {
        return write_read(addr, src, nw, 0, 0);
    }

    static inline uint8_t read(uint8_t addr, volatile uint8_t *dst, uint8_t nr)
    {
        return write_read(addr, 0, 0, dst, nr);
    }

    static uint8_t write_read(uint8_t addr, volatile const uint8_t *src, uint8_t nw, volatile uint8_t *dst, uint8_t nr)
    {
        if (s_busy)
            return s_err = 0xfe;                                    // busy

        s_err = 0;
        s_busy = true;
        s_addr = addr;
        s_nw = nw;
        s_nr = nr;
        s_src = src;
        s_dst = dst;

        TWCR = TWINT_TWEN_TWIE | _BV(TWSTA);                        // start condition
        return wait_idle();
    }

    static inline uint8_t wait_idle()
    {
        for (uint16_t i = 0; i < 100000; ++i)                       // wait up to 1 second
        {
            if (!s_busy)
                return s_err;
            delay_us(10);
        }
        return 0xff;                                                // time-out
    }

    static void isr()
    {
        switch (TWSR & 0xf8)
        {
        case TW_START: ;
        case TW_REP_START:
            TWDR = (s_addr << 1) | (s_src ? 0 : 1);                 // SLA+R/W
            TWCR = TWINT_TWEN_TWIE;                                 // send
            return;
        case TW_MT_SLA_ACK:                                         // from SLA+W
            if (s_nw)
            {
                TWDR = *s_src++;                                    // data
                TWCR = TWINT_TWEN_TWIE;                             // send
                return;
            }
            else
                break;                                              // stop
        case TW_MT_DATA_ACK:                                        // from data write
            if (--s_nw)
            {
                TWDR = *s_src++;                                    // data
                TWCR = TWINT_TWEN_TWIE;                             // send
            }
            else if (s_nr)
                TWCR = TWINT_TWEN_TWIE | _BV(TWSTA);                // repeated start condition
            else
                break;                                              // stop
            return;
        case TW_MR_DATA_ACK:
            *s_dst++ = TWDR;                                        // fall through, same code as SLA_ACK
        case TW_MR_SLA_ACK:
            TWCR = TWINT_TWEN_TWIE |  (--s_nr ? _BV(TWEA) : 0);     // ack if more to read
            return;
        case TW_MR_DATA_NACK:
            *s_dst++ = TWDR;
            break;                                                  // stop
        default:
            s_err = TWSR & 0xf8;                                    // stop + error
        }

        TWCR = _BV(TWINT) | _BV(TWSTO) | _BV(TWEN);                 // stop condition
        s_busy = false;
    }

private:
    static const uint8_t TWINT_TWEN_TWIE = _BV(TWINT) | _BV(TWEN) | _BV(TWIE);

    static volatile bool            s_busy;
    static volatile uint8_t            s_addr;
    static volatile uint8_t            s_nw;
    static volatile uint8_t            s_nr;
    static volatile uint8_t         s_err;
    static volatile const uint8_t    *s_src;
    static volatile uint8_t            *s_dst;
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

// Error handling suppory. User must provide required twi_error handler
// in scope before using this macro.
#define TWI(expr) do { \
    void twi_error(uint8_t err, const char *file, uint16_t line); \
    uint8_t _err = expr; \
    if (_err) \
        twi_error(_err, __FILE__, __LINE__); \
} while (false)

