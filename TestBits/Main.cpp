#include "../AVR/Bits.h"
#include "../AVR/Delay.h"

typedef pin_t<PD, 0> A;
typedef pin_t<PD, 1> B;
typedef pin_t<PD, 2> C;
typedef pin_t<PD, 3> D;
typedef pin_t<PD, 4> E;
typedef pin_t<PD, 5> F;
typedef pin_t<PD, 6> G;
typedef pin_t<PD, 7> H;

typedef bits_t<H, G, F, E, D, C, B, A> digit_t;

void setup()
{
	digital_out<A>();
	digital_out<B>();
	digital_out<C>();
	digital_out<D>();
	digital_out<E>();
	digital_out<F>();
	digital_out<G>();
	digital_out<H>();

	set<A>();
	set<B>();
	set<C>();
	set<D>();
	set<E>();
	set<F>();
	set<G>();
	set<H>();

	delay_ms(1000);

	clear<A>();
	clear<B>();
	clear<C>();
	clear<D>();
	clear<E>();
	clear<F>();
	clear<G>();
	clear<H>();

	delay_ms(1000);
}

void loop()
{
	static uint8_t i = 0;

	write_bits<digit_t>(i++);
	delay_ms(100);
}

int main()
{
	setup();
	for (;;)
		loop();
}

