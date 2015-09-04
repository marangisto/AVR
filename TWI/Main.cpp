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

void setup()
{
	seg7::setup();
	seg7::auto_refresh();

	seg7::write(0xbeef, 16);

	delay_ms(250);

	uint32_t twi_freq = 100000;

	TWSR = 0;								// set prescaler to 1
	TWBR = ((F_CPU / twi_freq) - 16) / 2;	// assuming prescaler 1
	TWCR = (1 << TWEN); // (1 << TWIE)		// FIXME: use interrupts too
}


// wait for TWINT and return TWSR status

static inline uint8_t twi_wait_int(uint8_t e)
{
	uint16_t i = 0;

	while ((TWCR & (1 << TWINT)) == 0)
		if (i++ > 10000)
			return 0xE0 | e;	// time-out
	return TWSR & 0xf8;
}

static void trace(uint8_t i)
{
	seg7::write(i);
	delay_ms(1000);
}

void loop()
{
//	static const uint8_t sla = 0x68;		// DS1307 address
	static const uint8_t sla = 0xA0;		// 24C32 address
	static uint8_t i = 0;

//	seg7::write("go");

	delay_ms(100);

	uint8_t sts = 0;

	TWCR = (1 << TWINT) | (1 << TWSTA) | (1 << TWEN);		// start condition
	if ((sts = twi_wait_int(0)) != TW_START)
		goto err;
#if 1

	TWDR = sla | 0;							// SLA+W
	TWCR = (1 << TWINT) | (1 << TWEN);		// send
	if ((sts = twi_wait_int(1)) != TW_MT_SLA_ACK)
		goto err;

	TWDR = 0;								// address byte 1
	TWCR = (1 << TWINT) | (1 << TWEN);		// send
	if ((sts = twi_wait_int(2)) != TW_MT_DATA_ACK)
		goto err;

	TWDR = i++;								// address byte 0
	TWCR = (1 << TWINT) | (1 << TWEN);		// send
	if ((sts = twi_wait_int(3)) != TW_MT_DATA_ACK)
		goto err;

	TWCR = (1 << TWINT) | (1 << TWSTA) | (1 << TWEN);		// repeated start condition
	if ((sts = twi_wait_int(4)) != TW_REP_START)
		goto err;


	TWDR = sla | 1;							// SLA+R
	TWCR = (1 << TWINT) | (1 << TWEN);		// send
	if ((sts = twi_wait_int(5)) != TW_MR_SLA_ACK)
		goto err;

	TWCR = (1 << TWINT) | (1 << TWEN);		// read
	if ((sts = twi_wait_int(6)) != TW_MR_DATA_NACK)
		goto err;

	seg7::write(TWDR);

#endif

#if 0
	TWDR = sla | 0;							// SLA+W
	TWCR = (1 << TWINT) | (1 << TWEN);		// send
	if ((sts = twi_wait_int(1)) != TW_MT_SLA_ACK)
		goto err;

	TWDR = 0;								// address byte 1
	TWCR = (1 << TWINT) | (1 << TWEN);		// send
	if ((sts = twi_wait_int(2)) != TW_MT_DATA_ACK)
		goto err;

	TWDR = i++;								// address byte 0
	TWCR = (1 << TWINT) | (1 << TWEN);		// send
	if ((sts = twi_wait_int(3)) != TW_MT_DATA_ACK)
		goto err;

	TWDR = ~i;								// data byte
	TWCR = (1 << TWINT) | (1 << TWEN);		// write
	if ((sts = twi_wait_int(4)) != TW_MT_DATA_ACK)
		goto err;

#endif

	TWCR = (1 << TWINT) | (1 << TWSTO) | (1 << TWEN);	// stop condition

//	seg7::write("done");

	delay_ms(50);

	return;

err:
	for (uint8_t i = 0; i < 5; ++i)
	{
		seg7::write(sts | 0xE000, 16);
		delay_ms(250);
		seg7::write("");
		delay_ms(100);
	}

	// reset TWI subsystem

	TWCR = 0;
	TWCR = (1 << TWEN); // (1 << TWIE)		// FIXME: use interrupts too
}

int main()
{
	setup();
	for (;;)
		loop();
}

