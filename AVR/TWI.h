#pragma once

#include <AVR/Pins.h>
#include <AVR/Delay.h>
#include <util/twi.h>
#include <avr/interrupt.h>

#if defined(__AVR_ATmega328PB__)
#define TWBR_0 TWBR0                // '_' becase TWBR0 is already defined on old AT***!
#else
#define TWINT0 TWINT
#define TWBR_0 TWBR
#define TWSR0 TWSR
#define TWCR0 TWCR
#define TWDR0 TWDR
#define TWAR0 TWAR
#define TWEA0 TWEA
#define TWEN0 TWEN
#define TWIE0 TWIE
#define TWSTA0 TWSTA
#define TWSTO0 TWSTO
#endif

template<int INST> struct twi_traits {};

template<>
struct twi_traits<0>
{
    static inline volatile uint8_t& twsr() { return TWSR0; }
    static inline volatile uint8_t& twbr() { return TWBR_0; }
    static inline volatile uint8_t& twcr() { return TWCR0; }
    static inline volatile uint8_t& twdr() { return TWDR0; }
    static inline volatile uint8_t& twar() { return TWAR0; }
    static const uint8_t twint = TWINT0;
    static const uint8_t twea = TWEA0;
    static const uint8_t twen = TWEN0;
    static const uint8_t twie = TWIE0;
    static const uint8_t twsta = TWSTA0;
    static const uint8_t twsto = TWSTO0;
};

#if defined(__AVR_ATmega328PB__)
template<>
struct twi_traits<1>
{
    static inline volatile uint8_t& twsr() { return TWSR1; }
    static inline volatile uint8_t& twbr() { return TWBR1; }
    static inline volatile uint8_t& twcr() { return TWCR1; }
    static inline volatile uint8_t& twdr() { return TWDR1; }
    static inline volatile uint8_t& twar() { return TWAR1; }
    static const uint8_t twint = TWINT1;
    static const uint8_t twea = TWEA1;
    static const uint8_t twen = TWEN1;
    static const uint8_t twie = TWIE1;
    static const uint8_t twsta = TWSTA1;
    static const uint8_t twsto = TWSTO1;
};
#endif

template<int INST>
class twi_master_t
{
public:
    typedef twi_traits<INST> inst;

    static inline void setup(uint32_t twi_freq = 100000)
    {
        inst::twsr() = 0;                                           // set prescaler to 1
        inst::twbr() = ((F_CPU / twi_freq) - 16) / 2;               // assuming prescaler 1
        inst::twdr() = 0xff;                                        // release SDA
        inst::twcr() = _BV(inst::twen);                             // enable TWI
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
        inst::twcr() = TWINT_TWEN_TWIE | _BV(inst::twsta);          // start condition
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
        switch (inst::twsr() & 0xf8)
        {
        case TW_START: ;
        case TW_REP_START:
            inst::twdr() = (s_addr << 1) | (s_src ? 0 : 1);         // SLA+R/W
            inst::twcr() = TWINT_TWEN_TWIE;                         // send
            return;
        case TW_MT_SLA_ACK:                                         // from SLA+W
            if (s_nw)
            {
                inst::twdr() = *s_src++;                            // data
                inst::twcr() = TWINT_TWEN_TWIE;                     // send
                return;
            }
            else
                break;                                              // stop
        case TW_MT_DATA_ACK:                                        // from data write
            if (--s_nw)
            {
                inst::twdr() = *s_src++;                            // data
                inst::twcr() = TWINT_TWEN_TWIE;                     // send
            }
            else if (s_nr)
                inst::twcr() = TWINT_TWEN_TWIE | _BV(inst::twsta);  // repeated start condition
            else
                break;                                              // stop
            return;
        case TW_MR_DATA_ACK:
            *s_dst++ = inst::twdr();                                // fall through, same code as SLA_ACK
        case TW_MR_SLA_ACK:
            inst::twcr() = TWINT_TWEN_TWIE |  (--s_nr ? _BV(inst::twea) : 0); // ack if more to read
            return;
        case TW_MR_DATA_NACK:
            *s_dst++ = inst::twdr();
            break;                                                  // stop
        default:
            s_err = inst::twsr() & 0xf8;                            // stop + error
        }

        inst::twcr() = _BV(inst::twint) | _BV(inst::twsto) | _BV(inst::twen); // stop condition
        s_busy = false;
    }

private:
    static const uint8_t TWINT_TWEN_TWIE = _BV(inst::twint) | _BV(inst::twen) | _BV(inst::twie);

    static volatile bool            s_busy;
    static volatile uint8_t            s_addr;
    static volatile uint8_t            s_nw;
    static volatile uint8_t            s_nr;
    static volatile uint8_t         s_err;
    static volatile const uint8_t    *s_src;
    static volatile uint8_t            *s_dst;
};

template<int INST> volatile bool twi_master_t<INST>::s_busy = false;
template<int INST> volatile uint8_t twi_master_t<INST>::s_addr;
template<int INST> volatile uint8_t twi_master_t<INST>::s_nw;
template<int INST> volatile uint8_t twi_master_t<INST>::s_nr;
template<int INST> volatile uint8_t twi_master_t<INST>::s_err;
template<int INST> volatile const uint8_t *twi_master_t<INST>::s_src;
template<int INST> volatile uint8_t *twi_master_t<INST>::s_dst;

template<int INST>
class twi_slave_t
{
public:
    typedef twi_traits<INST> inst;

    static inline void setup(uint8_t own_addr)
    {
        inst::twar() = own_addr << 1;       // own address
        inst::twcr() = _BV(inst::twen);     // enable TWI interface
        s_busy = false;
    }

    static void start_with_data(const uint8_t *msg, uint8_t len)
    {
        wait_idle();
        s_err = TW_NO_INFO;
        s_busy = true;
        s_len = len;
        for (volatile uint8_t *p = s_ptr = s_buf; len > 0; --len)
            *p++ = *msg++;
        inst::twcr() = TWINT_TWEN_TWIE_TWEA;
    }

    static const volatile uint8_t *get_buf() { return s_buf; }

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
        switch (inst::twsr())
        {
        case TW_ST_SLA_ACK:
            s_ptr = s_buf;
        case TW_ST_DATA_ACK:
            inst::twdr() = *s_ptr++;
            s_busy = true;
            inst::twcr() = TWINT_TWEN_TWIE_TWEA;
            break;
        case TW_ST_DATA_NACK:
            if (s_ptr - s_buf == s_len)
                ; // last_trans_ok = true;
            else
                s_err = inst::twsr();
            s_busy = false;
            inst::twcr() = TWINT_TWEN_TWIE_TWEA;
            break;
        case TW_SR_GCALL_ACK:
            // gen_addr_call = true;
        case TW_SR_SLA_ACK:
            //rx_data_in_s_buf = true;
            s_ptr = s_buf;
            s_busy = true;
            inst::twcr() = TWINT_TWEN_TWIE_TWEA;
            break;
        case TW_SR_DATA_ACK:
        case TW_SR_GCALL_DATA_ACK:
            *s_ptr++ = inst::twdr();
            // last_trans_ok = true;
            s_busy = true;
            inst::twcr() = TWINT_TWEN_TWIE_TWEA;
            break;
        case TW_SR_STOP:
            inst::twcr() = TWINT_TWEN_TWIE_TWEA;
            s_busy = false;
            break;
        case TW_SR_DATA_NACK:
        case TW_SR_GCALL_DATA_NACK:
        case TW_ST_LAST_DATA:
        case TW_BUS_ERROR:
            s_err = inst::twsr();
            inst::twcr() = _BV(inst::twsto) | _BV(inst::twint);     // release bus
            break;
        default:
            s_err = inst::twsr();
            s_busy = false;
            inst::twcr() = TWINT_TWEN_TWIE_TWEA;
        }
    }

private:
    static const uint8_t TWINT_TWEN_TWIE_TWEA = _BV(inst::twint) | _BV(inst::twen) | _BV(inst::twie) | _BV(inst::twea);

    static volatile uint8_t         s_buf[16];  // FIXME: parameterize size or use caller space?
    static volatile uint8_t         *s_ptr;
    static volatile uint8_t         s_len;
    static volatile bool            s_busy;
    static volatile uint8_t         s_err;
};

template<int INST> volatile bool twi_slave_t<INST>::s_busy = false;
template<int INST> volatile uint8_t twi_slave_t<INST>::s_buf[16];
template<int INST> volatile uint8_t *twi_slave_t<INST>::s_ptr;
template<int INST> volatile uint8_t twi_slave_t<INST>::s_len;
template<int INST> volatile uint8_t twi_slave_t<INST>::s_err;

