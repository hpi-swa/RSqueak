#include "file_chooser.h"
#include <FL/Fl_Native_File_Chooser.H>

int RSqueakOpenFileDialog_linux(char* szFile, int len) {
    Fl_Native_File_Chooser fnfc;
    fnfc.title("Pick a file");
    fnfc.type(Fl_Native_File_Chooser::BROWSE_FILE);
    fnfc.filter("Image\t*.image\n");
    // Show native chooser
    switch ( fnfc.show() ) {
    case -1:
    case  1:
	return 0;
    default:
	strncpy(szFile, fnfc.filename(), len - 1);
	return 1;
    }
}
