#include <loc.h>

LocType concat(LocType a, LocType b) {
    LocType r = {a.first_line, a.first_column, b.last_line, b.last_column};
    return r;
}