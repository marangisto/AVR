#include <Servo.h>

const int leftEye = A0;
const int rightEye = A1;

int leds[8] = { 5, 6, 7, 8, 9, 10, 11, 12 };
int nLeds =	sizeof(leds) / sizeof(*leds);

static Servo g_servo;
static int g_pos = 90;
static int g_l0 = 0;
static int g_r0 = 0;

int EyeVal(int eyePin)
{
	int v = analogRead(eyePin);
	return map(constrain(v, 25, 900), 25, 900, 1, 1000);
}	

void setup() {								
	Serial.begin(9600);
	for (int i = 0; i < nLeds; ++i)
		pinMode(leds[i], OUTPUT);
	g_servo.attach(4);	// PIN4
	g_servo.write(g_pos);
	delay(1000);							 // wait for a second
	g_l0 = EyeVal(leftEye);
	g_r0 = EyeVal(rightEye);
}

void loop() {
	static int i = 0;
	
	int l = EyeVal(leftEye) - g_l0;
	int r = EyeVal(rightEye) - g_r0;

	Serial.println(l);
	Serial.println(r);
	
	int j = i;
	i = ++i % nLeds;

	digitalWrite(leds[j], LOW);		// turn the LED off by making the voltage LOW
	digitalWrite(leds[i], HIGH);	 // turn the LED on (HIGH is the voltage level)

	if (r > l)
	{
		if (g_pos < 180)
			g_pos += 1;
	}
	else if (l > r)
	{
		if (g_pos > 0)
			g_pos -= 1;
	}

	g_servo.write(g_pos);

	delay(20);							 // wait for a second
}

