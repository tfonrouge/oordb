#include "oordb.ch"

/*
    EngineBase
*/
CLASS EngineBase FROM OORDBBASE
PROTECTED:
    DATA FRecNo
    DATA FStack    INIT {}
    DATA FStackLen INIT 0
ENDCLASS
