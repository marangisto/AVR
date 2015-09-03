#include <AVR/Pins.h>
#include <AVR/Bits.h>
#include <AVR/Delay.h>
#include <AVR/Seg7.h>

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

	uint32_t twi_freq = 50000;

	TWSR = 0;								// set prescaler to 1
	TWBR = ((F_CPU / twi_freq) - 16) / 2;	// assuming prescaler 1
	TWCR = (1 << TWEN); // (1 << TWIE)		// FIXME: use interrupts too
}

void loop()
{
	static const uint8_t sla = 0x68;		// DS1307 address
	static uint8_t i = 0;
	static uint8_t rw = 0;

//	rw = rw ? 0 : 1;

	delay_ms(1);

	TWCR |= (1 << TWSTA);					// start condition

	while ((TWCR & (1 << TWINT)) == 0)
		/* wait */ ;

	// assert TWSR == 0x08 - start transmitted

	rw = 0;	// write

	TWDR = (sla << 1) | rw;					// SLA+R/W

	TWCR |= (1 << TWINT);					// write

	while ((TWCR & (1 << TWINT)) == 0)
		/* wait */ ;

	// assert TWSR ==
	// 		0x20 - nack on SLA+W
	//      0x48 - nack on SLA+R

	seg7::write(TWSR, 16);

	TWCR &= ~(1 << TWSTA);					// clear start condition (otherwise we get repeat start)
	TWCR |= (1 << TWSTO);					// stop condition

	delay_ms(250);
}

int main()
{
	setup();
	for (;;)
		loop();
}

