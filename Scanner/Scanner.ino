#include <Wire.h>
#include <LiquidCrystal.h>

#include "LCDKeyPadShield.h"
#include "MotorShield.h"

static Stepper *gM0 = 0;
static Stepper *gM1 = 0;
static HBridge *gHB1 = 0;
static HBridge *gHB2 = 0;
static HBridge *gHB3 = 0;
static HBridge *gHB4 = 0;

void runPWM(HBridge *hb, HBridgeState s)
{
	hb->setPWM(0);
	hb->set(s);

	for (uint16_t w = 1; w <= 4096; w *= 2)
	{
		hb->setPWM(w);
		delay(50);
	}
}

void runHB(HBridge *hb)
{
	runPWM(hb, Forward);
	runPWM(hb, Reverse);
	hb->set(Off);
	delay(500);
}

void runHB(HBridge *hb, int n)
{
	for (uint8_t i = 0; i < n; ++i)
		runHB(hb);
}


LCDKeyPad gLCDKP;

void setup(){
	Serial.begin(9600);

	MotorShieldV2 *motorShield = new MotorShieldV2();

	gM0 = new Stepper(motorShield, Stepper1);
	gM1 = new Stepper(motorShield, Stepper2);
	gHB1 = new HBridge(motorShield, HBridge1);
	gHB2 = new HBridge(motorShield, HBridge2);
	gHB3 = new HBridge(motorShield, HBridge3);
	gHB4 = new HBridge(motorShield, HBridge4);

	LiquidCrystal& lcd = gLCDKP.lcd();

	delay(1000);
	lcd.setCursor(0,0);
	lcd.print("Push the buttons");
}
 
void loop(){
	LiquidCrystal& lcd = gLCDKP.lcd();

	lcd.setCursor(9,1);             // move cursor to second line "1" and 9 spaces over
	lcd.print(millis()/1000);       // display seconds elapsed since power-up
	lcd.setCursor(0,1);             // move to the begining of the second line

	switch (gLCDKP.read())
	{
		case KeyLeft:	lcd.print("Left  "); break;
		case KeyRight:	lcd.print("Right "); break;
		case KeyUp:		lcd.print("Up    "); break;
		case KeyDown:	lcd.print("Down  "); break;
		case KeySelect:	lcd.print("Select"); break;
		case KeyNone:	lcd.print("      "); break;
	}

	gM0->step();
	gM1->step();
	delay(100);

//	runHB(gHB1, 4);
//	runHB(gHB2, 4);
//	runHB(gHB3, 4);
//	runHB(gHB4, 4);
}

