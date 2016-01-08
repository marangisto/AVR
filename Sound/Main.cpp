#include "../AVR/Pins.h"
#include "../AVR/Delay.h"
#include "../AVR/Timer.h"

enum note_t
	{ C = 1911
	, C1 = 1804
	, D = 1703
	, Eb = 1607
	, E = 1517
	, F = 1432
	, F1 = 1352
	, G = 1276
	, Ab = 1204
	, A = 1136
	, Bb = 1073
	, B = 1012
	, c = 955
	, c1 = 902
	, d = 851
	, eb = 803
	, e = 758
	, f = 716
	, f1 = 676
	, g = 638
	, ab = 602
	, a = 568
	, bb = 536
	, b = 506
	};

// songs from https://therandombit.wordpress.com/2011/11/21/arduino-piezo-speaker-super-mario/

static note_t peer_gynt[] = { G, E, D, C, D, E, G, E, D, C, D, E, D, E,G, E, G, A, E, A, G, E, D, C };
static uint8_t _peer_gynt[] = { 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 4, 4, 4, 4, 8, 8, 8, 8, 8, 8, 8, 8, 8, 16 };

static note_t smoke[] = {E, G, A, E, G, Bb, A, E, G, A, G, E};
static uint8_t _smoke[] = {12, 12, 18, 12, 12, 6, 18, 12, 12, 18, 12, 24};
 
static note_t natal[] = {G, A, G, E, G, A, G, E, c, c, A, B, B, G, A, G, A, c, B, A, G, A, G, E};
static uint8_t _natal[] = {12, 4, 8, 16, 12, 4, 8, 16, 12, 4, 16, 12, 4, 16, 12, 4, 8, 8, 8, 8, 12, 4, 8, 16};
 
static note_t LTS[] = { Bb, G, G, Bb, G, G, Bb, G, G, Bb, G, G, Bb, G, C, G, Bb, G, G, Bb, G, G, Bb, G, G, Bb, G, G, Bb, G, F, D, F, D, G, F, D, C, Bb, G, Bb, C, C1, C, Bb, F, D, Bb, G, F, D, C, Bb, D, C, Bb, G };
static uint8_t _LTS[] = {4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4};

template<class AUDIO>
class sound_t
{
public:
	static void setup()
	{
		AUDIO::setup();
		timer::setup<normal_mode, top_default>();
		timer::start<8>();
		timer::isr(isr);
		timer::enable();
		period(2000);
		sei();
	}

	static void on(bool x) { s_on = x; }

	static void period(uint16_t us)
	{
		uint16_t n = (us * clocks_per_us) >> (1 + 3);	// half-period + prescaler
		s_count = 65536 - n;
	}

	static void freq(uint16_t f)
	{
		uint16_t n = (1000000 * clocks_per_us / f) >> (1 + 3);	// half-period + prescaler
		s_count = 65536 - n;
	}

	static void play(note_t x)
	{
		period(x);
		s_on = true;
	}

private:
	typedef timer_t<1> timer;

	static void isr()
	{
		timer::counter() = s_count;
		if (s_on)
			AUDIO::toggle();
	}

	static volatile bool s_on;
	static volatile uint16_t s_count;
};

template<class AUDIO>
volatile bool sound_t<AUDIO>::s_on = false;

template<class AUDIO>
volatile uint16_t sound_t<AUDIO>::s_count = 0;

typedef output_t<PC, 0> LED;
typedef output_t<PC, 1> AUDIO;
typedef sound_t<AUDIO> SOUND;

void setup()
{
	LED::setup();
	SOUND::setup();
}

void loop()
{
	static uint8_t i = 0;

	LED::toggle();
	
	SOUND::play(LTS[i]);

	for (uint8_t j = 0; j < _LTS[i]; ++j)
		delay_ms(50);

	if (++i >= sizeof(LTS) / sizeof(*LTS))
		i = 0;

}

int main()
{
	setup();
	for (;;)
		loop();
}

