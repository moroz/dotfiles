#include <stdlib.h>
#include <time.h>
#include "sunset.h"
#include <iostream>
#include <chrono>

#define DEFAULT_LAT "22.63429"
#define DEFAULT_LNG "120.36621"
#define UTC_OFFSET "8"

using namespace std;
using namespace std::chrono;

int get_minutes_from_local_midnight();

int main() {
  double lng, lat;
  const char* lngStr = getenv("LONGITUDE");
  if (lngStr == NULL) {
    lngStr = DEFAULT_LNG;
  }
  const char* latStr = getenv("LATITUDE");
  if (latStr == NULL) {
    latStr = DEFAULT_LAT;
  }
  lng = atof(lngStr);
  lat = atof(latStr);
  const char* offsetStr = getenv("UTC_OFFSET");
  if (offsetStr == NULL) {
    offsetStr = UTC_OFFSET;
  }
  const int offset = atoi(offsetStr);

  SunSet* s = new SunSet(lat, lng, offset);
  double sunrise = s->calcSunrise();
  double sunset = s->calcSunset();

  int minutes_from_midnight = get_minutes_from_local_midnight();

  if (minutes_from_midnight < sunrise || minutes_from_midnight > sunset) {
    cout << "NIGHT" << endl;
  } else {
    cout << "DAYTIME" << endl;
  }

  return 0;
}

int get_minutes_from_local_midnight() {
  const chrono::system_clock::time_point now = system_clock::now();
  auto now_timet = chrono::system_clock::to_time_t(now);
  auto now_local = localtime(&now_timet);

  auto midnight_local = now_local;
  midnight_local->tm_hour = 0;
  midnight_local->tm_min = 0;
  midnight_local->tm_sec = 0;
  auto midnight_timet = mktime(midnight_local);
  auto midnight = chrono::system_clock::from_time_t(midnight_timet);

  auto diff_mins = std::chrono::duration_cast<std::chrono::minutes>(now - midnight);

  return diff_mins.count();
}
