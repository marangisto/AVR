#include <avr/io.h>
#include <avr/interrupt.h>
#include "../AVR/Pins.h"

template<class T> T min(const T& x, const T& y) { return x < y ? x : y; }
template<class T> T max(const T& x, const T& y) { return x > y ? x : y; }

void delay_loop_2(uint16_t __count)
{
	__asm__ volatile
	(
		"1: sbiw %0,1" "\n\t"
		"brne 1b"
			: "=w" (__count)
			: "0" (__count)
	);
}

void delay(uint16_t n)
{
	while (n-- > 0)
		delay_loop_2(4000);
}

typedef bool dir_t;
static const bool Left = false;
static const bool Right = true;

typedef pin_t<PD,0> DIR;
typedef pin_t<PD,1> STEP;
typedef pin_t<PD,2> btnDn;
typedef pin_t<PD,3> btnUp;

static uint16_t profile[] =
//  { 20000, 12330, 9514, 7984, 6998, 6298, 5769, 5352, 5013, 4730
  { 4490, 4282, 4100, 3940, 3797, 3668, 3551, 3445, 3347, 3258
  , 3175, 3098, 3026, 2960, 2897, 2838, 2783, 2730, 2681, 2634
  , 2589, 2547, 2507, 2468, 2431, 2396, 2362, 2330, 2299, 2269
  , 2240, 2213, 2186, 2160, 2135, 2111, 2088, 2066, 2044, 2023
  , 2002, 1983, 1963, 1945, 1926, 1909, 1891, 1875, 1858, 1842
  , 1827, 1812, 1797, 1783, 1769, 1755, 1741, 1728, 1715, 1703
  , 1691, 1679, 1667, 1655, 1644, 1633, 1622, 1611, 1601, 1591
  , 1581, 1571, 1561, 1552, 1543, 1533, 1524, 1516, 1507, 1498
  , 1490, 1482, 1474, 1466, 1458, 1450, 1442, 1435, 1428, 1420
  , 1413, 1406, 1399, 1392, 1386, 1379, 1372, 1366, 1360, 1353
  , 1347, 1341, 1335, 1329, 1323, 1317, 1312, 1306, 1300, 1295
  , 1289, 1284, 1279, 1274, 1268, 1263, 1258, 1253, 1248, 1243
  , 1239, 1234, 1229, 1225, 1220, 1215, 1211, 1206, 1202, 1198
  , 1193, 1189, 1185, 1181, 1177, 1173, 1169, 1165, 1161, 1157
  , 1153, 1149, 1145, 1141, 1138, 1134, 1130, 1127, 1123, 1120
  , 1116, 1113, 1109, 1106, 1102, 1099, 1096, 1092, 1089, 1086
  , 1083, 1079, 1076, 1073, 1070, 1067, 1064, 1061, 1058, 1055
  , 1052, 1049, 1046, 1043, 1040, 1038, 1035, 1032, 1029, 1027
  , 1024, 1021, 1018, 1016, 1013, 1011, 1008, 1005, 1003, 1000
  };

static uint16_t profile_size = sizeof(profile) / sizeof(*profile);
static volatile uint16_t n_steps = 0;    // tell isr how many steps to run
static volatile uint16_t cur_step = 0;   // current isr step
static volatile uint16_t max_i = 0;        // max speed
static volatile uint8_t limit_mask = 0;
static volatile bool inflight = false;

static const uint16_t micro_steps = 4;  // micro-steps shifts

void runStepper(dir_t dir, uint16_t n)
{
    cli();                     // disable global interrupts
    n_steps = n << micro_steps;
    cur_step = 0;
    max_i = min(profile_size - 1, n / 10);    // FIXME: do we want this?
    inflight = true;
    write<DIR>(dir == Left);
//    limit_mask = (dir == Right) ? B00000100 : B00001000;			// FIXME!
    TCCR1A = 0;                // set entire TCCR1A register to 0
    TCCR1B = 0;                // same for TCCR1B
    TCNT1 = 0;                 // max delay to 1st pulse
//    TCCR1B |= (1 << CS10);
    TCCR1B |= (1 << CS11);
//    TCCR1B |= (1 << CS12);
    TIMSK1 |= (1 << TOIE1);     // enable timer overflow interrupt
    sei();

    while (inflight)
        delay(100);
}

void homePosition()
{
    runStepper(Right, 4000);
    delay(500);
    runStepper(Left, 200);
    delay(100);
}

ISR(TIMER1_OVF_vect)
{
    if ((cur_step < n_steps)) // FIXME: LIMITS =  && !(PIND & limit_mask))
    {
		uint16_t c = cur_step, m = max_i;
        uint16_t i = min(c, n_steps - c) >> 1;  // update once per step cycle

        TCNT1 = 65535 - (profile[min(i, m)] >> 2);
        toggle<STEP>();
        ++cur_step;
    } else
        inflight = false;
}

void setup()
{
	digital_in<btnDn, btnUp>();
	set<btnDn, btnUp>(); 			// pull-ups
	digital_out<DIR, STEP>();

	homePosition();
}

void loop()
{
    const uint16_t w = 380;
    const uint16_t et = 500;
    
    for (uint8_t i = 0; i < 5; ++i)
    {
      runStepper(Left, w);
      delay(et);
    }
    runStepper(Right, 5 * w);
    delay(1000);
}

int main()
{
	setup();
	for (;;)
		loop();
}

