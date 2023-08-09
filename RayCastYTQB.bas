'---------------------------------------------------------------------------
'---                                                                     ---
'---                Motor Grafico pseudo 3D (Ray Cast)                   ---
'---                                                                     ---
'---                 Programado por: Juan Eguia, 2023                    ---
'---                                                                     ---
'===========================================================================
'---                       C O N S T A N T E S                           ---
'---------------------------------------------------------------------------
Const blanco = _RGB32(245, 245, 245)
Const gris = _RGB32(70, 75, 75)
Const pared_oscuro = _RGB32(150, 150, 150)
Const pared_claro = _RGB32(128, 128, 128)
Const negro = _RGB32(0, 0, 0)
Const amarillo = _RGB32(249, 216, 28)
Const verde = _RGB32(83, 216, 61)
Const rojo = _RGB32(255, 55, 0)
Const azul_oscuro = _RGB32(22, 11, 78)
Const azul_celeste = _RGB32(83, 183, 255)

Const TILE_X = 20
Const TILE_Y = 20
Const NRO_COLUMNAS = 40
Const NRO_FILAS = 30
Const RES_X = 600
Const RES_Y = 600
Const FPS = 120

Const pi = 3.14159265
Const RENDER_CON_TEXTURA = 1
Const DISTANCIA_ENTRE_RAYOS = 1
Const NRO_RAYOS = RES_X

Const FOV = 60 '60grados FOV (campo de vision del jugador)

Const NRO_TEXTURAS_PARED = 14


'===========================================================================
'---                      Variables  O B J E T O S
'---------------------------------------------------------------------------
Type jugador
    x As Single
    y As Single
    ancho As Integer
    alto As Integer
    avanza As Integer
    gira As Integer
    anguloRotacion As Single
    velGiro As Single
    velMovimiento As Single
End Type

Type rayo
    x As Single
    y As Single
    anguloRotacion As Single
    incrementoAngulo As Single
    angulo As Single
    wallHitX As Single
    wallHitY As Single
    wallHitXHorizontal As Single
    wallHitYHorizontal As Single
    wallHitXVertical As Single
    wallHitYVertical As Single
    columna As Single
    distancia As Single
    pixelTextura As Integer
    idTextura As Integer
    valorTH As Integer
    valorTV As Integer
    distanciaPlanoProyeccion As Single
    hCamara As Single
End Type


'-----------------------------------------------------------------------
'---                   D I M E N S I O N A R                         ---
'-----------------------------------------------------------------------
Dim jugador As jugador
Dim rayo(RES_X) As rayo
Dim escenario(NRO_COLUMNAS, NRO_FILAS) As Integer

Dim a As Integer
Dim b As Integer
Dim renderTexturas As _Bit
Dim ciclos As Integer
Dim cadencia As Integer
Dim gameOver As _Bit

Dim Shared modo As _Bit
Dim Shared distanciaEntrePuntos As Single
Dim Shared colorPared As Long

Dim texturaPared(NRO_TEXTURAS_PARED) As Long

'------------------------------------------------------------------------------------
'---            ESCENARIO 1
'------------------------------------------------------------------------------------
Data 1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,2,3,1,1,1,1,1,1,1,1,1,1,1,1,2,3,1,1,1,1,1,1,1,1
Data 1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,1
Data 1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,1
Data 1,0,0,0,0,1,0,0,0,0,0,0,1,1,1,0,0,0,0,0,0,1,0,0,0,0,0,0,0,0,0,0,0,0,0,1,1,0,0,1
Data 1,0,0,0,0,1,0,0,0,0,0,0,1,1,1,0,0,0,0,0,0,1,0,0,0,0,0,0,0,0,0,0,0,0,0,1,1,0,0,1
Data 1,0,0,0,0,1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,1
Data 1,0,0,1,1,1,1,0,0,0,0,0,0,0,0,0,0,0,0,0,1,1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,1
Data 1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,1,1,0,0,0,0,0,0,0,0,0,0,0,0,0,1,1,0,0,1
Data 2,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,1,1,0,0,1
Data 3,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,1
Data 1,0,0,0,0,0,0,0,0,1,0,0,0,0,0,0,0,0,1,1,1,1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,1
Data 1,0,0,0,0,0,0,0,0,1,0,0,0,0,0,0,0,0,1,1,1,1,0,0,0,0,0,0,0,0,0,0,0,0,0,1,1,0,0,1
Data 1,0,0,0,0,0,0,0,0,1,0,0,0,0,0,0,0,0,0,0,0,1,0,0,0,0,0,0,0,0,0,0,0,0,0,1,1,0,0,1
Data 2,0,0,0,0,0,0,0,0,1,0,0,0,0,0,0,0,0,0,0,0,1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,1
Data 3,0,0,0,0,0,0,0,0,1,0,0,0,0,0,0,0,0,0,0,0,1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,1
Data 1,0,0,0,0,0,0,0,0,1,0,0,0,0,0,0,0,0,0,0,0,1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,1
Data 1,0,0,0,0,0,0,0,0,1,0,0,0,0,0,0,0,0,0,0,0,1,0,0,0,0,0,0,0,0,0,0,0,0,0,1,1,0,0,1
Data 1,0,0,0,0,0,0,0,0,1,0,0,0,0,0,0,0,0,0,0,0,1,0,0,0,0,0,0,0,0,0,0,0,0,0,1,1,0,0,1
Data 1,1,1,1,1,1,1,1,1,1,1,1,1,0,0,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1
Data 1,0,0,0,0,0,0,0,0,1,0,0,1,0,0,1,0,0,0,0,0,1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,1
Data 2,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,1
Data 3,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,1,1,1,1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,1
Data 1,0,0,0,0,0,0,0,0,1,0,0,0,0,0,0,0,0,1,1,1,1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,1
Data 1,1,0,0,0,0,0,0,0,1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,2
Data 1,0,0,0,0,0,0,0,0,1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,3
Data 1,0,0,0,0,0,0,0,0,1,0,0,0,0,0,0,0,0,1,1,1,1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,1
Data 1,1,0,0,0,0,0,0,0,1,0,0,0,0,0,0,0,0,1,1,1,1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,1
Data 1,0,0,0,0,0,0,0,0,1,0,0,0,0,0,0,0,0,0,0,0,1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,1
Data 1,0,0,0,0,0,0,0,0,1,0,0,0,0,0,0,0,0,0,0,0,1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,1
Data 1,1,1,1,2,3,1,1,1,1,1,1,1,0,0,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1

'===============================================================
'--------                                               --------
'--------            INICIALIZACION GENERAL             --------
'--------                                               --------
'---------------------------------------------------------------
Screen _NewImage(RES_X, RES_Y, 32)
_Title " RayCast (pseudo 3D) by Juan Eguia "
_ScreenMove _DesktopWidth / 2 - _Width / 2, _DesktopHeight / 2 - _Height / 2

_PrintMode _KeepBackground
Randomize Timer

Cls

'updatesSonidos
updatesGraficos
updatesGenerales

'============================================================
'--------                                            --------
'--------      B U C L E   P R I N C I P A L         --------
'--------                                            --------
'============================================================
Do
    _Limit FPS
    'PCopy _Display, 1

    If _KeyDown(9) Then cambia_modo cadencia 'Pulsando TAB cambia 2D/3D
    If _KeyDown(84) Or _KeyDown(116) Then cambia_render cadencia
    If _KeyDown(27) Then gameOver = -1

    dibuja_escenario

    el_jugador
    el_rayo

    mostrar_marcadores

    ciclos = ciclos + 1

    If ciclos >= 32000 Then ciclos = 1
    If cadencia > 0 Then cadencia = cadencia - 1

    _Display
    'PCopy 1, _Display

Loop Until gameOver

'===================================================================
'---                   F I N   P R O G R A M A                   ---
'===================================================================
'salir
Beep
System

'===================================================================
'---                                                             ---
'---                    S U B R U T I N A S                      ---
'---                                                             ---
'-------------------------------------------------------------------
Sub dibuja_escenario

    Dim x As Integer
    Dim y As Integer

    Shared escenario() As Integer

    If Not modo Then
        Line (0, 0)-Step(RES_X, RES_Y), gris, BF

        For y = 1 To NRO_FILAS
            For x = 1 To NRO_COLUMNAS
                If escenario(x, y) = 0 Then Line (x * TILE_X, y * TILE_Y)-Step(TILE_X, TILE_Y), gris, BF
                If escenario(x, y) > 0 Then Line (x * TILE_X, y * TILE_Y)-Step(TILE_X, TILE_Y), pared_claro, BF
            Next x
        Next y
    Else
        Line (0, 0)-Step(RES_X, Int(RES_Y / 2)), azul_celeste, BF
        Line (0, Int(RES_Y / 2))-Step(RES_X, RES_Y), gris, BF
    End If

End Sub

'===================================================================
Sub el_jugador

    Shared jugador As jugador

    sub_leer_teclado jugador
    sub_actualiza jugador
    sub_dibuja jugador

End Sub

'-------------------------------------------------------------------
Sub sub_leer_teclado (jugador As jugador)

    Dim tecla As Long

    tecla = _KeyHit

    If tecla = 18432 Then jugador.avanza = 1
    If tecla = -18432 Then jugador.avanza = 0

    If tecla = 20480 Then jugador.avanza = -1
    If tecla = -20480 Then jugador.avanza = 0

    If tecla = 19200 Then jugador.gira = -1
    If tecla = -19200 Then jugador.gira = 0

    If tecla = 19712 Then jugador.gira = 1
    If tecla = -19712 Then jugador.gira = 0

End Sub

'-------------------------------------------------------------------
Sub sub_actualiza (jugador As jugador)

    Dim x As Single
    Dim y As Single

    Dim nuevaX As Single
    Dim nuevaY As Single

    nuevaX = jugador.x + jugador.avanza * Cos(jugador.anguloRotacion) * jugador.velMovimiento
    nuevaY = jugador.y + jugador.avanza * Sin(jugador.anguloRotacion) * jugador.velMovimiento

    x = Int(nuevaX / TILE_X)
    y = Int(nuevaY / TILE_Y)

    If Not colision_escenario(x, y) Then
        jugador.x = nuevaX
        jugador.y = nuevaY
    End If

    jugador.anguloRotacion = jugador.anguloRotacion + jugador.gira * jugador.velGiro
    normaliza_angulo jugador

End Sub

'-------------------------------------------------------------------
Sub sub_dibuja (jugador As jugador)

    Dim xDestino As Single
    Dim yDestino As Single
    Dim ancho As Integer
    Dim alto As Integer

    ancho = jugador.ancho
    alto = jugador.alto

    If Not modo Then
        Line (jugador.x - Int(ancho / 2), jugador.y - Int(alto / 2))-Step(ancho, alto), blanco, BF

        'xDestino = jugador.x * Cos(jugador.anguloRotacion) * 0.2
        'yDestino = jugador.y * Sin(jugador.anguloRotacion) * 0.2

        'Line (jugador.x, jugador.y)-Step(xDestino, yDestino), rojo
    End If

End Sub

'========================================================================
Sub el_rayo

    Dim a As Integer

    Dim whx As Single
    Dim why As Single
    Dim distanciaHorizontal As Single
    Dim distanciaVertical As Single
    Dim distanciaEntrePuntos As Single
    Dim casilla As Integer

    Shared rayo() As rayo
    Shared jugador As jugador

    '-----------------------------------------------------
    For a = 1 To NRO_RAYOS
        sub_actualiza_rayo rayo(), a 'ACTUALIZAR rayo antes

        '-------------------------------------------------------------------
        ' CHEQUEAMOS CUAL es mas CORTO... HORIZONTAL / VERTICAL??
        '-------------------------------------------------------------------
        distanciaHorizontal = 9999 'Inicializamos EXAGERADAMENTE por defecto
        distanciaVertical = 9999

        If sub_colisionesHorizontales(rayo(), a) Then
            whx = rayo(a).wallHitXHorizontal
            why = rayo(a).wallHitYHorizontal
            distanciaEntrePuntos = calculaDistanciaEntrePuntos(whx, why, rayo(a).x, rayo(a).y)
            distanciaHorizontal = distanciaEntrePuntos
        End If

        If sub_colisionesVerticales(rayo(), a) Then
            whx = rayo(a).wallHitXVertical
            why = rayo(a).wallHitYVertical
            distanciaEntrePuntos = calculaDistanciaEntrePuntos(whx, why, rayo(a).x, rayo(a).y)
            distanciaVertical = distanciaEntrePuntos
        End If

        If distanciaHorizontal < distanciaVertical Then
            rayo(a).wallHitX = rayo(a).wallHitXHorizontal
            rayo(a).wallHitY = rayo(a).wallHitYHorizontal
            rayo(a).distancia = distanciaHorizontal
            colorPared = pared_claro
            'LINEA DE LA TEXTURA a dibujar -----------------
            casilla = Int(rayo(a).wallHitX / TILE_X) * TILE_X
            rayo(a).pixelTextura = rayo(a).wallHitX - casilla
            rayo(a).idTextura = 0 + rayo(a).valorTH
        Else
            rayo(a).wallHitX = rayo(a).wallHitXVertical
            rayo(a).wallHitY = rayo(a).wallHitYVertical
            rayo(a).distancia = distanciaVertical
            colorPared = pared_oscuro
            'LINEA DE LA TEXTURA a dibujar -----------------
            casilla = Int(rayo(a).wallHitY / TILE_Y) * TILE_Y
            rayo(a).pixelTextura = rayo(a).wallHitY - casilla
            rayo(a).idTextura = 0 + rayo(a).valorTV + 10
        End If

        corregir_ojoDePez rayo(), a

        sub_dibuja_rayo rayo(), a

    Next a

End Sub

'-------------------------------------------------------------------
Sub corregir_ojoDePez (rayo() As rayo, a As Integer)

    rayo(a).distancia = rayo(a).distancia * Cos(rayo(a).anguloRotacion - rayo(a).angulo)

End Sub

'-------------------------------------------------------------------
Function sub_colisionesHorizontales (rayo() As rayo, a As Integer)

    Dim colisionH As _Bit

    Dim xIntercept As Single
    Dim yIntercept As Single

    Dim xStep As Single
    Dim yStep As Single

    Dim adyacente As Single

    Dim siguienteXHorizontal As Single
    Dim siguienteYHorizontal As Single

    Dim casillaX As Integer
    Dim casillaY As Integer

    '---------------------------------------
    sub_colisionesHorizontales = 0
    colisionH = 0

    xIntercept = 0
    yIntercept = 0

    xStep = 0
    yStep = 0

    ' Buscamos la 1ra INSTERSECCION HORIZONTAL
    yIntercept = Int(rayo(a).y / TILE_Y) * TILE_Y

    ' Si apunta hacia ABAJO, INCREMENTAMOS 1Tile
    If abajo(rayo(), a) Then yIntercept = yIntercept + TILE_Y

    ' Se le SUMA el CATETO ADYACENTE
    adyacente = (yIntercept - rayo(a).y) / Tan(rayo(a).angulo)
    xIntercept = rayo(a).x + adyacente

    ' Calcular los STEPs
    yStep = TILE_Y
    xStep = yStep / Tan(rayo(a).angulo)

    ' Si vamos a la IZQUIERDA o ARRIBA, el paso es NEGATIVO
    If Not abajo(rayo(), a) Then yStep = -yStep

    If (izquierda(rayo(), a) And xStep > 0) Or (Not izquierda(rayo(), a) And xStep < 0) Then xStep = -xStep

    siguienteXHorizontal = xIntercept
    siguienteYHorizontal = yIntercept

    ' Si apunta hacia ARRIBA, forzamos 1 PIXEL EXTRA
    If Not abajo(rayo(), a) Then siguienteYHorizontal = siguienteYHorizontal - 1

    '-------------------------------------------------------------------
    ' BUCLE para BUSCAR el PTO de COLISION (Horizontal)
    '-------------------------------------------------------------------
    Do
        casillaX = Int(siguienteXHorizontal / TILE_X)
        casillaY = Int(siguienteYHorizontal / TILE_Y)

        If colision_escenario(casillaX, casillaY) Then
            sub_colisionesHorizontales = -1
            colisionH = -1
            rayo(a).valorTH = valorTile(casillaX, casillaY)
            rayo(a).wallHitXHorizontal = siguienteXHorizontal
            rayo(a).wallHitYHorizontal = siguienteYHorizontal
        Else
            siguienteXHorizontal = siguienteXHorizontal + xStep
            siguienteYHorizontal = siguienteYHorizontal + yStep
        End If

    Loop Until colisionH

End Function

'-------------------------------------------------------------------
Function sub_colisionesVerticales (rayo() As rayo, a As Integer)

    Dim colisionV As _Bit

    Dim xIntercept As Single
    Dim yIntercept As Single

    Dim xStep As Single
    Dim yStep As Single

    Dim opuesto As Single

    Dim siguienteXVertical As Single
    Dim siguienteYVertical As Single

    Dim casillaX As Integer
    Dim casillaY As Integer

    '---------------------------------------
    sub_colisionesVerticales = 0
    colisionV = 0

    xIntercept = 0
    yIntercept = 0

    xStep = 0
    yStep = 0

    ' Buscamos la 1ra INSTERSECCION VERTICAL
    xIntercept = Int(rayo(a).x / TILE_X) * TILE_X

    ' Si apunta hacia la DCHA, INCREMENTAMOS 1 Tile
    If Not izquierda(rayo(), a) Then xIntercept = xIntercept + TILE_X

    ' Se le SUMA el CATETO OPUESTO
    opuesto = (xIntercept - rayo(a).x) * Tan(rayo(a).angulo)
    yIntercept = rayo(a).y + opuesto

    ' Calcular los STEPs
    xStep = TILE_X

    'SI vamos a la IZQUIERDA, INVERTIMOS
    If izquierda(rayo(), a) Then xStep = -xStep

    yStep = TILE_X * Tan(rayo(a).angulo)

    If (Not abajo(rayo(), a) And yStep > 0) Or (abajo(rayo(), a) And yStep < 0) Then yStep = -yStep

    siguienteXVertical = xIntercept
    siguienteYVertical = yIntercept

    ' Si apunta hacia la IZQUIERDA, forzamos -1 PIXEL
    If izquierda(rayo(), a) Then siguienteXVertical = siguienteXVertical - 1

    '-------------------------------------------------------------------
    ' BUCLE para BUSCAR el PTO de COLISION (Vertical)
    '-------------------------------------------------------------------
    Do
        casillaX = Int(siguienteXVertical / TILE_X)
        casillaY = Int(siguienteYVertical / TILE_Y)

        If colision_escenario(casillaX, casillaY) Then
            sub_colisionesVerticales = -1
            colisionV = -1
            rayo(a).valorTV = valorTile(casillaX, casillaY)
            rayo(a).wallHitXVertical = siguienteXVertical
            rayo(a).wallHitYVertical = siguienteYVertical
        Else
            siguienteXVertical = siguienteXVertical + xStep
            siguienteYVertical = siguienteYVertical + yStep
        End If

    Loop Until colisionV

End Function

'-------------------------------------------------------------------
Sub sub_actualiza_rayo (rayo() As rayo, a As Integer)

    Shared jugador As jugador

    rayo(a).anguloRotacion = jugador.anguloRotacion
    rayo(a).angulo = rayo(a).angulo + jugador.gira * jugador.velGiro
    normaliza_angulo_rayo rayo(), a

    rayo(a).x = jugador.x
    rayo(a).y = jugador.y

End Sub

'-------------------------------------------------------------------
Sub sub_dibuja_rayo (rayo() As rayo, a As Integer)

    Dim xDestino As Single
    Dim yDestino As Single

    xDestino = rayo(a).wallHitX
    yDestino = rayo(a).wallHitY

    If Not modo Then
        Line (rayo(a).x, rayo(a).y)-(xDestino, yDestino), verde
    Else
        sub_render_pared rayo(), a
    End If

End Sub

'-------------------------------------------------------------------
Sub sub_render_pared (rayo() As rayo, a As Integer)

    Dim altoTile As Single
    Dim alturaMuro As Single

    Dim y0 As Single
    Dim y1 As Single
    Dim x As Integer

    Dim altura As Single
    Dim altoTextura As Single
    Dim alturaTextura As Single
    Dim idTxt As Single

    Shared renderTexturas As _Bit
    Shared texturaPared() As Long

    altoTile = RES_Y 'RES_Y es lo que aprendi (por defecto)
    alturaMuro = (altoTile / rayo(a).distancia) * rayo(a).distanciaPlanoProyeccion

    y0 = RES_Y / 2 - Int(alturaMuro / 2)
    y1 = y0 + alturaMuro
    x = rayo(a).columna

    altura = 0
    altoTextura = 64
    alturaTextura = y0 - y1

    If Not renderTexturas Then
        Line (x, y0)-(x, y1), colorPared 'renderiza lineas
    Else
        idTxt = rayo(a).idTextura
        _PutImage (x, y0)-(x, y1), texturaPared(idTxt), , (rayo(a).pixelTextura, 0)-Step(0, altoTextura)
    End If

End Sub

'===================================================================
Function colision_escenario (x As Integer, y As Integer)

    Shared escenario() As Integer

    colision_escenario = 0

    If x > NRO_COLUMNAS Or y > NRO_FILAS Or x < 1 Or y < 1 Then
        colision_escenario = -1
        Exit Function
    End If

    If escenario(x, y) > 0 Then colision_escenario = -1

End Function


'===================================================================
Function calculaDistanciaEntrePuntos (jugX, jugY, sprX, sprY)

    calculaDistanciaEntrePuntos = 0
    calculaDistanciaEntrePuntos = Sqr((sprX - jugX) * (sprX - jugX) + (sprY - jugY) * (sprY - jugY))

End Function

'===================================================================
Function valorTile (x As Integer, y As Integer)

    Shared escenario() As Integer

    valorTile = 0

    If x > NRO_COLUMNAS Or y > NRO_FILAS Or x < 1 Or y < 1 Then
        valorTile = 1
        Exit Function
    End If

    valorTile = escenario(x, y)

End Function

'===================================================================
Function abajo (rayo() As rayo, a As Integer)

    abajo = 0
    If rayo(a).angulo < _Pi Then abajo = -1

End Function

'-------------------------------------------------------------------
Function izquierda (rayo() As rayo, a As Integer)

    izquierda = 0
    If rayo(a).angulo > _Pi / 2 And rayo(a).angulo < _Pi * 1.5 Then izquierda = -1

End Function

'===================================================================
Sub normaliza_angulo (jugador As jugador)

    If jugador.anguloRotacion < 0 Then
        jugador.anguloRotacion = jugador.anguloRotacion + 2 * _Pi
        Exit Sub
    End If

    If jugador.anguloRotacion > 2 * _Pi Then
        jugador.anguloRotacion = jugador.anguloRotacion - 2 * _Pi
        Exit Sub
    End If

End Sub

'===================================================================
Sub normaliza_angulo_rayo (rayo() As rayo, a As Integer)

    If rayo(a).angulo < 0 Then
        rayo(a).angulo = rayo(a).angulo + 2 * _Pi
        Exit Sub
    End If

    If rayo(a).angulo > 2 * _Pi Then
        rayo(a).angulo = rayo(a).angulo - 2 * _Pi
        Exit Sub
    End If

End Sub

'===================================================================
Sub cambia_render (cadencia As Integer)

    Shared renderTexturas As _Bit

    If cadencia <= 0 Then
        If Not renderTexturas Then renderTexturas = -1 Else renderTexturas = 0
        cadencia = 20
        soniquete 100, 600
    End If

End Sub

'===================================================================
Sub cambia_modo (cadencia As Integer)

    If cadencia <= 0 Then
        If Not modo Then modo = -1 Else modo = 0 ' 0 = 2D ... -1 = 3D
        cadencia = 20
        soniquete 100, 700
    End If

End Sub

'=======================================================================
Sub mostrar_marcadores

    'Shared mostrarFPS As Integer
    Shared renderTexturas As _Bit

    'Color amarillo
    'Locate 1, 1
    'Print " Fps: ";

    'Color blanco
    'Print LTrim$(Str$(mostrarFPS))

    Color amarillo
    Locate 1, 11
    Print " TAB: ";

    Color blanco
    If Not modo Then Print "3D" Else Print "2D"

    Color amarillo
    Locate 1, 21
    Print " T: ";

    Color blanco
    If Not renderTexturas Then Print "Texturas ON" Else Print "Texturas OFF"

    Color amarillo
    Locate 1, 41
    Print " Alt+ENTER: ";

    Color blanco
    Print "Pantalla Completa"

End Sub

'=======================================================================
Sub soniquete (uno As Integer, dos As Integer)

    Dim a As Integer
    For a = uno To dos Step 50
        Sound a, 0.2
    Next a

End Sub

'===================================================================
Sub updatesGraficos

    Dim a As Integer

    Shared texturaPared() As Long

    For a = 1 To NRO_TEXTURAS_PARED
        If a < 4 Or (a > 10 And a < 14) Then
            texturaPared(a) = _LoadImage("muroLadrillo_rojo" + LTrim$(Str$(a)) + ".png")
        End If
    Next a

End Sub

'===================================================================
Sub updatesGenerales

    Shared ciclos As Integer
    Shared cadencia As Integer
    Shared gameOver As _Bit
    Shared renderTexturas As _Bit

    Shared jugador As jugador
    Shared rayo() As rayo
    Shared escenario() As Integer

    renderTexturas = 0
    modo = 0 '0 = 2D (Mapa) ... -1 = 3D (Variable Global)
    ciclos = 0
    cadencia = 0
    gameOver = 0

    instancia_jugador jugador
    instancia_rayos rayo(), jugador
    instancia_escenario escenario()

End Sub

'-------------------------------------------------------------------
Sub instancia_jugador (jugador As jugador)

    jugador.x = TILE_X * 9
    jugador.y = TILE_Y * 5
    jugador.ancho = 6
    jugador.alto = 6

    jugador.anguloRotacion = 40 * (_Pi / 180)
    jugador.gira = 0
    jugador.avanza = 0
    jugador.velGiro = 3 * (_Pi / 180)
    jugador.velMovimiento = 3

End Sub

'-------------------------------------------------------------------
Sub instancia_rayos (rayo() As rayo, jugador As jugador)

    Dim a As Integer

    Dim medioFOV As Single
    Dim anguloInicial As Single

    medioFOV = FOV / 2

    For a = 1 To NRO_RAYOS
        rayo(a).x = jugador.x
        rayo(a).y = jugador.y

        rayo(a).anguloRotacion = jugador.anguloRotacion
        rayo(a).incrementoAngulo = (FOV / NRO_RAYOS) * (_Pi / 180)

        anguloInicial = rayo(a).anguloRotacion - medioFOV * (_Pi / 180)
        rayo(a).angulo = anguloInicial + rayo(a).incrementoAngulo * a

        rayo(a).wallHitX = 0
        rayo(a).wallHitY = 0
        rayo(a).wallHitXHorizontal = 0
        rayo(a).wallHitYHorizontal = 0
        rayo(a).wallHitXVertical = 0
        rayo(a).wallHitYVertical = 0

        rayo(a).columna = a
        rayo(a).distancia = 0
        rayo(a).pixelTextura = 0
        rayo(a).idTextura = 0
        rayo(a).valorTH = 0
        rayo(a).valorTV = 0
        rayo(a).distanciaPlanoProyeccion = (RES_X / 2) / Tan(medioFOV)
        rayo(a).hCamara = 0
    Next a

End Sub

'-------------------------------------------------------------------
Sub instancia_escenario (escenario() As Integer)

    Dim y As Integer
    Dim x As Integer
    Dim pared As Integer

    Restore
    For y = 1 To NRO_FILAS
        For x = 1 To NRO_COLUMNAS
            Read pared
            escenario(x, y) = pared
        Next x
    Next y

End Sub









