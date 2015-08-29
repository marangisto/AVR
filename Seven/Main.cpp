#include "../AVR/Bits.h"
#include "../AVR/Delay.h"

typedef pin_t<PD, 7> D0;
typedef pin_t<PB, 1> D1;
typedef pin_t<PC, 2> D2;
typedef pin_t<PD, 3> D3;

typedef pin_t<PD, 4> A;
typedef pin_t<PB, 2> B;
typedef pin_t<PD, 5> C;
typedef pin_t<PD, 1> D;
typedef pin_t<PD, 0> E;
typedef pin_t<PC, 3> F;
typedef pin_t<PD, 6> G;
typedef pin_t<PD, 2> DP;

typedef bits_t<A, B, C, D, E, F, G> digit_t;

void setup()
{
	digital_out<D0>();
	digital_out<D1>();
	digital_out<D2>();
	digital_out<D3>();

	set<D0>();
	set<D1>();
	set<D2>();
	set<D3>();

	digital_out<A>();
	digital_out<B>();
	digital_out<C>();
	digital_out<D>();
	digital_out<E>();
	digital_out<F>();
	digital_out<G>();
	digital_out<DP>();

	set<A>();
	set<B>();
	set<C>();
	set<D>();
	set<E>();
	set<F>();
	set<G>();
	set<DP>();
}

static void display(uint8_t x)
{
	write<A>((x & (1 << 0)) == 0);
	write<B>((x & (1 << 1)) == 0);
	write<C>((x & (1 << 2)) == 0);
	write<D>((x & (1 << 3)) == 0);
	write<E>((x & (1 << 4)) == 0);
	write<F>((x & (1 << 5)) == 0);
	write<G>((x & (1 << 6)) == 0);
}

void loop()
{
	static uint8_t j = 0, k = 0;

	switch (k & 0x3)
	{
		case 0: set<D3>(); break;
		case 1: set<D0>(); break;
		case 2: set<D1>(); break;
		case 3: set<D2>(); break;
	}

	switch (k & 0x3)
	{
		case 0: write_bits<digit_t>(~0b11110001); break;
		case 1: write_bits<digit_t>(~0b11111001); break;
		case 2: write_bits<digit_t>(~0b11111001); break;
		case 3: write_bits<digit_t>(~0b11111100); break;
	}

	switch (k & 0x3)
	{
		case 0: clear<D0>(); break;
		case 1: clear<D1>(); break;
		case 2: clear<D2>(); break;
		case 3: clear<D3>(); break;
	}

	++k;

	delay_ms(1);
}

int main()
{
	setup();
	for (;;)
		loop();
}

