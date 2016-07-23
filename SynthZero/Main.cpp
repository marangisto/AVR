#include <AVR/Delay.h>
#include <AVR/Timer.h>

typedef timer_t<0> W;
typedef timer_t<1> P;
typedef output_t<PD, 7> CK;
typedef input_t<PD, 0, enable_pullup> BT0;

static const unsigned long wsteps = 32;
typedef int wave_t[wsteps];

static const wave_t wsin = { 128,152,176,198,218,234,245,253,255,253,245,234,218,198,176,152,128,103,79,57,37,21,10,2,0,2,10,21,37,57,79,103 };
static const wave_t wsaw = { 0,8,16,25,33,41,49,58,66,74,82,90,99,107,115,123,132,140,148,156,165,173,181,189,197,206,214,222,230,239,247,255 };
static const wave_t wtri = { 0,16,32,48,64,80,96,112,128,143,159,175,191,207,223,239,255,239,223,207,191,175,159,143,128,112,96,80,64,48,32,16 };
static const wave_t wsqr = { 0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255 };

enum note_t
    { A3    = 220
    , As3   = 233
    , B3    = 247
    , C4    = 262
    , Cs4   = 277
    , D4    = 294
    , Ds4   = 311
    , E4    = 330
    , F4    = 349
    , Fs4   = 370
    , G4    = 392
    , Gs4   = 415
    , A4    = 440
    , As4   = 466
    , B4    = 494
    , C5    = 523
    , Cs5   = 554
    , D5    = 587
    , Ds5   = 622
    , E5    = 659
    , F5    = 698
    , Fs5   = 740
    , G5    = 784
    , Gs5   = 831
    , A5    = 880
    , As5   = 932
    , B5    = 988
    , C6    = 1046
    , Cs6   = 1109
    , D6    = 1175
    , Ds6   = 1245
    , E6    = 1319
    , F6    = 1397
    , Fs6   = 1480
    , G6    = 1568
    , Gs6   = 1661
    , A6    = 1760
    };

static volatile int g_period = 0; // time for 32nd of full cycle
static volatile const int *wave = wsqr;

static void isr()
{
    static unsigned i = 0;

    P::counter() = 65535 - g_period + 65;
    W::output_compare_register<channel_a>() = wave[i];
    if (++i == wsteps)
    {
        i = 0;
        CK::toggle();
    }
}

void setup()
{
    //W::setup<pwm_phase_correct, top_0xff>();
    W::setup<fast_pwm, top_0xff>();
    W::clock_select<1>();
    W::output_pin<channel_a>::setup();
    W::compare_output_mode<channel_a, clear_on_compare_match>();

    P::setup<normal_mode>();
    P::clock_select<1>();
	P::isr(isr);
	P::enable();
	sei();

	CK::setup();
    BT0::setup();
}


void loop()
{
    static int i = 0;
    static int w = 0;
    static bool bt0 = true;
 
    if (BT0::read())
    {
        switch (++w % 4)
        {
            case 0: wave = wsin; break;
            case 1: wave = wsaw; break;
            case 2: wave = wtri; break;
            case 3: wave = wsqr; break;
        }
    }


    note_t n = A3;

    switch (i++ % 4)
    {
        case 0: n = A3; break;
        case 1: n = A4; break;
        case 2: n = A5; break;
        case 3: n = A6; break;
    }

    g_period = F_CPU / (wsteps * n);

	delay_ms(2000);
}

int main()
{
	setup();
	for (;;)
		loop();
}

