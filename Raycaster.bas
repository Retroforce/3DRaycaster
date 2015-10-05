
    ' Declare global properties
    global mainDC, textureDC, memoryDC, compatibleBMP

    nomainwin

    '   Map
    dim map$(24, 24)

    gosub [load_map]

    let posX = 22.0
    let posY = 12.0
    let dirX = -1.0
    let dirY = 0.0
    let planeX = 0
    let planeY = 0.66
    let time = 0
    let oldTime = 0

    WindowWidth = 512
    WindowHeight = 330

    resolusion = 4

    open "Raycaster" for graphics_nsb as #main
    #main, "trapclose [xit]"
    #main, "down; fill 185 122 87; flush"

    loadbmp "texture", "textures\tex.bmp"
    call initDisplay, hwnd(#main),  hbmp("texture"), WindowWidth, WindowHeight
    gosub [load_pens]

[loop]
scan
goto [redraw]
wait


[redraw]

    for x = 0 to WindowWidth step resolusion

        cameraX = 2.0*x / WindowWidth-1
        rayPosX = posX
        rayPosY = posY
        rayDirX = dirX + planeX *cameraX
        rayDirY = dirY + planeY *cameraX

        mapX = int(rayPosX)
        mapY = int(rayPosY)
        sideDistX = 0.0
        sideDistY = 0.0

        deltaDistX = sqr((1+(rayDirY*rayDirY)/(rayDirX*rayDirX)))

        if rayDirY*rayDirY > 0 then
        ' need to find a better way to fix the devide by zero crash here
        deltaDistY = sqr((1+(rayDirX*rayDirX)/(rayDirY*rayDirY)))
        end if
        perpWallDist = 0.0

        stepX = 0
        stepY = 0

        hit = 0

        steps = 1

        if rayDirX < 0 then
            stepX = -1*steps
            sideDistX = (rayPosX - mapX) * deltaDistX
        else
            stepX = steps
            sideDistX = (mapX + 1.0 - rayPosX) * deltaDistX
        end if

        if rayDirY < 0 then
            stepY = -1*steps
            sideDistY = (rayPosY - mapY) *deltaDistY
        else
            stepY = steps
            sideDistY = (mapY + 1.0 - rayPosY) *deltaDistY
        end if

        ' perform DDA
        while hit = 0
            'jump to next map square, OR in x-direction, OR in y-direction
            if sideDistX < sideDistY then
            sideDistX = sideDistX + deltaDistX
            mapX = mapX + stepX
            side = 0
            else
            sideDistY = sideDistY + deltaDistY
            mapY = mapY + stepY
            side = 1
            end if

            ' Check if ray has hit a wall
            if val(map$(mapX, mapY)) > 0 then
                hit = 1
            end if
        wend

        '   Calculate distance projected on camera direction (oblique distance will give fisheye effect!)
        if side = 0 then
            perpWallDist = abs((mapX - rayPosX + (1 - stepX) / 2) / rayDirX)
        else
            perpWallDist = abs((mapY - rayPosY + (1 - stepY) / 2) / rayDirY)
        end if

        '   Calculate height of line to draw on screen
        lineHeight = abs(int(WindowHeight / perpWallDist))

        ' calculate lowest and highest pixel to fill in current stripe
        drawStart = int(-1*lineHeight / 2 + WindowHeight / 2)

        if drawStart < 0 then drawStart = 0
        drawEnd = int(lineHeight / 2 + WindowHeight / 2)
        if drawEnd > WindowHeight or drawEnd = WindowHeight then drawEnd = WindowHeight - 1

    'if map$(mapX, mapY) = "3" then
        ' texturing calculations
      texNum = int(val(map$(mapX, mapY)) - 1)

      ' calculate value of wallX
      wallX = 0.0
      if side = 1 then
      wallX = rayPosX + ((mapY - rayPosY + (1 - stepY) / 2) / rayDirY) * rayDirX
      wallX = wallX - Floor(wallX)
      else
      wallX = rayPosY + ((mapX - rayPosX + (1 - stepX) / 2) / rayDirX) * rayDirY
      wallX = wallX - Floor(wallX)
      end if

      ' x coordinate on the texture
      texX = int(wallX * 64)
      if side = 0 and rayDirX > 0 then
        texX = texWidth - texX - 1
        texX = -1*texX
      end if

      if side = 1 and rayDirY < 0 then
        texX = texWidth - texX - 1
        texX = -1*texX
    end if

      'if texX > 2  then texX = 63
      'if texX < 1 then texX = 1

      if texX = 0 then texX = 255

    height = drawEnd - drawStart


    if side = 1 then
    if map$(mapX, mapY) = "1" then
        texLocation = 343 + texX
        CallDll #gdi32, "StretchBlt", memoryDC as ulong, x as long, drawStart as long, resolusion as long, height as long, textureDC as ulong, texLocation as long, 0 as long, resolusion as long, 63 as long, _SRCCOPY as ulong, result as boolean
    end if
    if map$(mapX, mapY) = "2" then
        texLocation = 534 + texX
        CallDll #gdi32, "StretchBlt", memoryDC as ulong, x as long, drawStart as long, resolusion as long, height as long, textureDC as ulong, texLocation as long, 0 as long, resolusion as long, 63 as long, _SRCCOPY as ulong, result as boolean
    end if
    if map$(mapX, mapY) = "3" then
        texLocation = 150 + texX
        CallDll #gdi32, "StretchBlt", memoryDC as ulong, x as long, drawStart as long, resolusion as long, height as long, textureDC as ulong, texLocation as long, 0 as long, resolusion as long, 63 as long, _SRCCOPY as ulong, result as boolean
    end if
    if map$(mapX, mapY) = "4" then
        texLocation = 407 + texX
        CallDll #gdi32, "StretchBlt", memoryDC as ulong, x as long, drawStart as long, resolusion as long, height as long, textureDC as ulong, texLocation as long, 0 as long, resolusion as long, 63 as long, _SRCCOPY as ulong, result as boolean
    end if
    else
        if map$(mapX, mapY) = "1" then
            loc = 343 + texX
            CallDll #gdi32, "StretchBlt", memoryDC as ulong, x as long, drawStart as long, resolusion as long, height as long, textureDC as ulong, loc as long, 64 as long, resolusion as long, 63 as long, _SRCCOPY as ulong, result as boolean
        end if
        if map$(mapX, mapY) = "2" then
            loc = 534 + texX
            CallDll #gdi32, "StretchBlt", memoryDC as ulong, x as long, drawStart as long, resolusion as long, height as long, textureDC as ulong, loc as long, 64 as long, resolusion as long, 63 as long, _SRCCOPY as ulong, result as boolean
        end if
        if map$(mapX, mapY) = "3" then
            loc = 150 + 64 + texX
            CallDll #gdi32, "StretchBlt", memoryDC as ulong, x as long, drawStart as long, resolusion as long, height as long, textureDC as ulong, loc as long, 64 as long, resolusion as long, 63 as long, _SRCCOPY as ulong, result as boolean
        end if
        if map$(mapX, mapY) = "4" then
            loc = 407 + texX
            CallDll #gdi32, "StretchBlt", memoryDC as ulong, x as long, drawStart as long, resolusion as long, height as long, textureDC as ulong, loc as long, 64 as long, resolusion as long, 63 as long, _SRCCOPY as ulong, result as boolean
        end if
    end if

'    else
'
'        global mainDC, textureDC, memoryDC, compatibleBMP
'
'            if map$(mapX, mapY) = "1" then CallDLL #gdi32,"SelectObject", memoryDC As long, redPen As long, hMem As long
'            if map$(mapX, mapY) = "2" then CallDLL #gdi32,"SelectObject", memoryDC As long, greenPen As long, hMem As long
'            if map$(mapX, mapY) = "3" then CallDLL #gdi32,"SelectObject", memoryDC As long, bluePen As long, hMem As long
'            if map$(mapX, mapY) = "4" then CallDLL #gdi32,"SelectObject", memoryDC As long, whitePen As long, hMem As long
'
'        if side = 1 then
'            if map$(mapX, mapY) = "1" then CallDLL #gdi32,"SelectObject", memoryDC As long, redDarkPen As long, hMem As long
'            if map$(mapX, mapY) = "2" then CallDLL #gdi32,"SelectObject", memoryDC As long, greenDarkPen As long, hMem As long
'            if map$(mapX, mapY) = "3" then CallDLL #gdi32,"SelectObject", memoryDC As long, blueDarkPen As long, hMem As long
'            if map$(mapX, mapY) = "4" then CallDLL #gdi32,"SelectObject", memoryDC As long, whiteDarkPen As long, hMem As long
'        end if
'
   '     struct Point, x as long, y as long
   '     CallDLL #gdi32, "MoveToEx", memoryDC As long, x As long, drawStart As long, Point As struct, r As boolean
   '     CallDLL #gdi32, "LineTo", memoryDC As long, x As long, drawEnd As long, r As boolean
   '
   ' end if

    next x

        if frameTime > 0 then
            fp$ = "FPS: ";str$(int(1.0 / frameTime))
            textCount = len(fp$)
            CallDLL #gdi32, "TextOutA", memoryDC As long, 10 As long, 10 As long, fp$ As ptr, textCount As long, r As long
        end if

    gosub [draw_sprites]

'   Transfer buffer to screen
CallDll #gdi32, "BitBlt", _
    mainDC as ulong,_     'The destination DC = graphicbox
    0 as long,_        'x location on destination
    0 as long,_        'y location on destination
    WindowWidth as long,_      'width to transfer
    WindowHeight as long,_      'height to transfer
    memoryDC as ulong,_  'The source DC = memory
    0 as long,_        'x location in source
    0 as long,_        'y location in source
    _SRCCOPY as ulong,_'The operation to be performed
    result as boolean  'nonzero if successful

'   Clear memory buffer
CallDll #gdi32, "PatBlt", memoryDC as ulong, 0 as long, 0 as long, 512 as long, WindowHeight as long, _BLACKNESS as ulong, result as boolean

'   Draw Sky
CallDll #gdi32, "StretchBlt", _
    memoryDC as ulong,_ 'handle to destination DC
    0 as long,_    'ulx location on destination
    0 as long,_    'uly location on destination
    WindowWidth as long,_     'width to stretch to
    160 as long,_     'height to stretch to
    textureDC as ulong,_  'handle to source DC
    151 as long,_     'ulx location in source
    129 as long,_     'uly location in source
    507 as long,_     'width to take from source
    150 as long,_     'height to take from source
    _SRCCOPY as ulong,_   'The operation to be performed
    result as boolean  'nonzero if successful

'   Draw floor
CallDll #gdi32, "StretchBlt", _
    memoryDC as ulong,_ 'handle to destination DC
    0 as long,_    'ulx location on destination
    160 as long,_    'uly location on destination
    WindowWidth as long,_     'width to stretch to
    320 as long,_     'height to stretch to
    textureDC as ulong,_  'handle to source DC
    151 as long,_     'ulx location in source
    396 as long,_     'uly location in source
    506 as long,_     'width to take from source
    400 as long,_     'height to take from source
    _SRCCOPY as ulong,_   'The operation to be performed
    result as boolean  'nonzero if successful



    '   timing for input and FPS counter
    oldTime = time
    'calldll #kernel32, "GetTickCount", ticks as long
    time = time$("ms")
    frameTime = (time - oldTime) / 1000.0 ' frameTime is the time this frame has taken, in seconds

    ' speed modifiers
    moveSpeed = frameTime * 5.0 ' the constant value is in squares/second
    rotSpeed = frameTime * 3.0' `the constant value is in radians/second

    gosub [key]

    goto [loop]

[key]
    calldll #user32, "GetAsyncKeyState", _VK_LEFT as short, r as short
    if r < 0 then

      ' both camera direction and camera plane must be rotated
      oldDirX = dirX
      dirX = dirX * cos(rotSpeed) - dirY * sin(rotSpeed)
      dirY = oldDirX * sin(rotSpeed) + dirY * cos(rotSpeed)
      oldPlaneX = planeX
      planeX = planeX * cos(rotSpeed) - planeY * sin(rotSpeed)
      planeY = oldPlaneX * sin(rotSpeed) + planeY * cos(rotSpeed)

    end if

    calldll #user32, "GetAsyncKeyState", _VK_RIGHT as short, r as short
    if r < 0 then

      ' both camera direction and camera plane must be rotated
      oldDirX = dirX
      dirX = dirX * cos(-1*rotSpeed) - dirY * sin(-1*rotSpeed)
      dirY = oldDirX * sin(-1*rotSpeed) + dirY * cos(-1*rotSpeed)
      oldPlaneX = planeX
      planeX = planeX * cos(-1*rotSpeed) - planeY * sin(-1*rotSpeed)
      planeY = oldPlaneX * sin(-1*rotSpeed) + planeY * cos(-1*rotSpeed)

    end if

    calldll #user32, "GetAsyncKeyState", _VK_UP as short, r as short
    if r < 0 then
        'print "Y: ";map$(int(posX), int(posY + dirY * moveSpeed)), int(posX), int(posY + dirY * moveSpeed), side
        print "X: ";map$(int(posX + dirX * moveSpeed), int(posY)), int(posX), int(posX + dirX * moveSpeed), side


        if map$(int(posX + dirX+2 * moveSpeed), int(posY)) = "0" then posX = posX + dirX * moveSpeed
        if map$(int(posX), int(posY + dirY+2 * moveSpeed)) = "0" then posY = posY + dirY * moveSpeed

    end if

    calldll #user32, "GetAsyncKeyState", _VK_DOWN as short, r as short
    if r < 0 then
      if map$(int(posX - dirX * moveSpeed), posY) = "0" then posX = posX - dirX * moveSpeed
      if map$(posX, int(posY - dirY * moveSpeed)) = "0" then posY = posY - dirY * moveSpeed
    end if

    calldll #user32, "GetAsyncKeyState", _VK_SHIFT as short, r as short
    if r < 0 then
    end if
    return

[load_map]

data 1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1, 9
data 1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,1, 9
data 1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,1, 9
data 1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,1, 9
data 1,0,0,0,0,0,2,2,2,2,2,0,0,0,0,3,0,3,0,3,0,0,0,1, 9
data 1,0,0,0,0,0,2,0,0,0,2,0,0,0,0,0,0,0,0,0,0,0,0,1, 9
data 1,0,0,0,0,0,2,0,0,0,2,0,0,0,0,3,0,0,0,3,0,0,0,1, 9
data 1,0,0,0,0,0,2,0,0,0,2,0,0,0,0,0,0,0,0,0,0,0,0,1, 9
data 1,0,0,0,0,0,2,2,0,2,2,0,0,0,0,3,0,3,0,3,0,0,0,1, 9
data 1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,1, 9
data 1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,1, 9
data 1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,1, 9
data 1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,1, 9
data 1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,1, 9
data 1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,1, 9
data 1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,1, 9
data 1,4,4,4,4,4,4,4,4,0,0,0,0,0,0,0,3,0,0,0,0,0,0,1, 9
data 1,4,0,4,0,0,0,0,4,0,0,0,0,0,0,0,0,0,0,0,0,0,0,1, 9
data 1,4,0,0,0,0,3,0,4,0,0,0,0,0,0,0,0,0,0,0,0,0,0,1, 9
data 1,4,0,4,0,0,0,0,4,0,0,0,0,0,0,0,0,0,0,0,0,0,0,1, 9
data 1,4,0,4,4,4,4,4,4,0,0,0,0,0,0,0,0,0,0,0,0,0,0,1, 9
data 1,4,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,1, 9
data 1,4,4,4,4,4,4,4,4,0,0,0,0,0,0,0,0,0,0,0,0,0,0,1, 9
data 1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1, 8

    x = 0: y = 0

    while value<>8

    read value

    if value < 8 then
        map$(x, y) = str$(value)
    end if

    x = x + 1
    if value = 9 then
        x = 0
        y = y + 1
    end if

wend

return


function Floor(x)
  if x>=0 or x=int(x) then
    Floor = int(x)
  else
    Floor = int(x-1)
  end if
end function

    [xit]
    calldll #gdi32,"DeleteDC",memDC as long,r as ulong
    calldll #gdi32,"DeleteObject",redPen as long,r as ulong
    calldll #gdi32,"DeleteObject",greenPen as long,r as ulong
    calldll #gdi32,"DeleteObject",bluePen as long,r as ulong
    calldll #gdi32,"DeleteObject",whitePen as long,r as ulong
    calldll #gdi32,"DeleteObject",redDarkPen as long,r as ulong
    calldll #gdi32,"DeleteObject",greenDarkPen as long,r as ulong
    calldll #gdi32,"DeleteObject",blueDarkPen as long,r as ulong
    calldll #gdi32,"DeleteObject",whiteDarkPen as long,r as ulong
    calldll #user32,"ReleaseDC",hG as long,hDC as long,r as ulong
    close #main
    end


'========================== Functions

Function GetDC(hWnd)
    CallDLL #user32, "GetDC",_
        hWnd As Ulong,_  'window or control handle
        GetDC As Ulong   'returns device context
    End Function

Sub ReleaseDC hWnd, hDC
    CallDLL#user32,"ReleaseDC",_
        hWnd As Ulong,_  'window or control handle
        hDC As Ulong,_   'handle of DC to delete
        result As Long
    End Sub

Function CreateCompatibleDC(hDC)
    CallDLL #gdi32,"CreateCompatibleDC",_
        hDC As Ulong,_               'window DC
        CreateCompatibleDC As Ulong  'memory DC
    End Function

Function CreateCompatibleBitmap(hDC, width, height)
    calldll #gdi32, "CreateCompatibleBitmap",_
        hDC AS ulong,_                  'window DC, NOT memory DC
        width AS long,_                 'width of created bitmap
        height AS long,_                'height of created bitmap
        CreateCompatibleBitmap AS ulong 'returns handle if successful
End Function

Sub DeleteDC hDC
        CallDLL #gdi32, "DeleteDC",_
        hDC As Ulong,_   'memory DC to delete
        result As Boolean
    End Sub

Sub StretchBlt hDCdest,x,y,w,h,hDCsrc,x2,y2,w2,h2
    CallDLL #gdi32, "SetStretchBltMode",_
        hDCdest As Ulong,_       'device context
        _COLORONCOLOR As Long,_ 'color reduction mode
        result As Long
    CallDLL #gdi32, "StretchBlt",_
        hDCdest As Ulong,_   'destination
        x As Long,_         'destination x pos
        y As Long,_         'destination y pos
        w As Long,_         'destination width desired
        h As Long,_         'destination height desired
        hDCsrc As Ulong,_    'source
        x2 As Long,_        'x location to start from source
        y2 As Long,_        'y location to start from source
        w2 As Long,_        'width desired from source
        h2 As Long,_        'height desired from source
        _SRCCOPY As Ulong,_ 'dwRasterOperation
        result As Boolean
    End Sub

Sub TransparentBlt hDCdest, x, y, w, h, hDCsrc, x2, y2, w2, h2, color
    open "Msimg32.dll" for DLL as #msimg
    CallDll #msimg, "TransparentBlt",_
        hDCdest as ulong,_     'The destination DC = graphicbox
        x as long,_    'x location on destination
        y as long,_    'y location on destination
        w as long,_    'width to stretch to
        h as long,_    'height to stretch to
        hDCsrc as ulong,_  'The source DC = memory
        x2 as long,_     'x location in source
        y2 as long,_     'y location in source
        w2 as long,_     'width to take from source
        h2 as long,_     'height to take from source
        color as ulong,_  'RGB/long color value to make transparent
        result as boolean  'nonzero if successful
        close #msimg
end sub

Function SelectObject(hDC,hObject)
    CallDLL #gdi32,"SelectObject",_
        hDC As Ulong,_        'memory device context
        hObject As Ulong,_    'handle of object
        SelectObject As Ulong 'returns previously selected object
    End Function

Function SetPixel(hdc,x,y,rgbColor)
    CallDll #gdi32, "SetPixel",_
        hdc as Ulong,_       'the handle of the Device context from GetDC
        x as long,_         'the x coordinate to draw the pixel
        y as long,_         'the y coordinate to draw the pixel
        rgbColor as long,_
        SetPixel as long
    End Function



'========================================== Sub routines
sub initDisplay display, texture, width, height
    mainDC = GetDC(display)
    memoryDC = CreateCompatibleDC(mainDC)
    textureDC = CreateCompatibleDC(mainDC)
    compatibleBMP = CreateCompatibleBitmap(mainDC, width, height)
    object = SelectObject(memoryDC, compatibleBMP)
    object = SelectObject(textureDC, texture)
end sub

[load_pens]
    redColor = 255 + 0 * 256 + 0 * 256*256
    greenColor = 0 + 255 * 256 + 21 * 256*256
    blueColor = 27 + 0 * 256 + 255 * 256*256
    whiteColor = 255 + 255 * 256 + 255 * 256*256
    darkRedColor = 200 + 0 * 256 + 0 * 256*256
    darkGreenColor = 0 + 200 * 256 + 21 * 256*256
    darkBlueColor = 27 + 0 * 256 + 200 * 256*256
    darkWhiteColor = 200 + 200 * 256 + 200 * 256*256

    calldll #gdi32, "CreatePen", 0 As Long, 1 As Long, redColor As Long, redPen as Long
    calldll #gdi32, "CreatePen", 0 As Long, 1 As Long, greenColor As Long, greenPen as Long
    calldll #gdi32, "CreatePen", 0 As Long, 1 As Long, blueColor As Long, bluePen as Long
    calldll #gdi32, "CreatePen", 0 As Long, 1 As Long, whiteColor As Long, whitePen as Long
    calldll #gdi32, "CreatePen", 0 As Long, 1 As Long, darkRedColor As Long, redDarkPen as Long
    calldll #gdi32, "CreatePen", 0 As Long, 1 As Long, darkGreenColor As Long, greenDarkPen as Long
    calldll #gdi32, "CreatePen", 0 As Long, 1 As Long, darkBlueColor As Long, blueDarkPen as Long
    calldll #gdi32, "CreatePen", 0 As Long, 1 As Long, darkWhiteColor As Long, whiteDarkPen as Long
return

[draw_sprites]
    '   Draw gun
    call TransparentBlt memoryDC, 300, 200, 100, 100, textureDC, 0, 0, 150, 170, whiteColor
return
