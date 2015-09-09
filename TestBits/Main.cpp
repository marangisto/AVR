#include <AVR/Pins.h>
#include <AVR/Delay.h>

typedef output_t<PD, 0> A;
typedef output_t<PD, 1> B;
typedef output_t<PD, 2> C;
typedef output_t<PD, 3> D;
typedef output_t<PD, 4> E;
typedef output_t<PD, 5> F;
typedef output_t<PD, 6> G;
typedef output_t<PD, 7> H;

typedef bits_t<H, G, F, E, D, C, B, A> digit_t;

void setup()
{
	A::setup();
	B::setup();
	C::setup();
	D::setup();
	E::setup();
	F::setup();
	G::setup();
	H::setup();
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

