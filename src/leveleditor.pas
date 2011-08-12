unit LevelEditor;

{$mode objfpc}{$H+}

interface

type
  pmapPieceEnum = ^mapPieceEnum;
  mapPieceEnum = (ROCK, TREE, CRATE, HOUSE, BUNGALOW, BAR, SKYSCRAPER, STATUE,
               SACK_OF_CATNIP, CAT_SPAWNPOINT, SPAMBOT_SPAWNPOINT, MECH_SPAWNPOINT);

procedure runLevelEditor;

implementation

uses SysUtils, Classes, Math, TypInfo, Crt, SMSDL, Input, Display, ShaderClass,
     GameActorClass, SceneryClass, SackClass, SpawnPointClass, GlobalGameVariables;

const
  FIRST_PIECE = ROCK;
  LAST_PIECE = MECH_SPAWNPOINT;

var
  currentMapPiece : mapPieceEnum;
  placedPieces, placedPieceEnums : TList;
  xPos, zPos : real;
  ghostPiece : PGameActor = nil;

procedure setupEditorMatrices;
begin
  setMatrix(MODELVIEW_MATRIX);
  copyMatrix(ORTHOGRAPHIC_MATRIX, PROJECTION_MATRIX);
  copyMatrix(IDENTITY_MATRIX, MODELVIEW_MATRIX);
  translateMatrix(xPos, zPos, -200);
  rotateMatrix(90, 1, 0, 0);
end;

procedure placeMapPiece;
  function rotationToMouse(actor : PGameActor) : real;
  var
    xDiff, zDiff : real;
  begin
    xDiff := (mouseX-xPos)-actor^.x;
    if (xDiff = 0) then xDiff := 1;
    zDiff := (-mouseY+zPos)-actor^.z;
    result := radToDeg(arcTan(zDiff/xDiff));
    if (result < 0) then result := -result-90 else result := -result+90;
    if (zDiff > 0) then result += 180;
  end;

var
  newMapPiece, tempGameActor : PGameActor;
  newMapPieceEnum, tempMapPieceEnum : pmapPieceEnum;
  i : integer;
  collision : boolean = false;
begin
  case currentMapPiece of
  ROCK : newMapPiece := new(PRock, create(mouseX-xPos, -mouseY+zPos, 0));
  TREE : newMapPiece := new(PTree, create(mouseX-xPos, -mouseY+zPos, 0));
  CRATE : newMapPiece := new(PCrate, create(mouseX-xPos, -mouseY+zPos, 0));
  HOUSE : newMapPiece := new(PHouse, create(mouseX-xPos, -mouseY+zPos, 0));
  BUNGALOW : newMapPiece := new(PBungalow, create(mouseX-xPos, -mouseY+zPos, 0));
  BAR : newMapPiece := new(PBar, create(mouseX-xPos, -mouseY+zPos, 0));
  SKYSCRAPER : newMapPiece := new(PSkyScraper, create(mouseX-xPos, -mouseY+zPos, 0));
  STATUE : newMapPiece := new(PStatue, create(mouseX-xPos, -mouseY+zPos, 0));
  SACK_OF_CATNIP :
    begin
      if (placedPieces.count > 0) then
      begin
        for i := 0 to placedPieces.count-1 do
        begin
          tempGameActor := PGameActor(placedPieces[i]);
          if (tempGameActor^.name = 'sack') then
          begin
            clrscr;
            writeln('Only one sack of catnip is allowed to be placed. Delete the old sack of catnip to place a new one');
            exit;
          end;
        end;
      end;
      newMapPiece := new(PSack, create(mouseX-xPos, -mouseY+zPos, 0));
    end;
  CAT_SPAWNPOINT : newMapPiece := new(PCatSpawnPoint, create(mouseX-xPos, -mouseY+zPos, 0));
  SPAMBOT_SPAWNPOINT : newMapPiece := new(PSpambotSpawnPoint, create(mouseX-xPos, -mouseY+zPos, 0));
  MECH_SPAWNPOINT : newMapPiece := new(PMechSpawnPoint, create(mouseX-xPos, -mouseY+zPos, 0));
  otherwise exit;
  end;

  sleep(200);

  if (placedPieces.count > 0) then
  begin
    for i := 0 to placedPieces.count-1 do
    begin
      tempGameActor := PGameActor(placedPieces[i]);
      if (newMapPiece^.collision(tempGameActor)) then
      begin
        collision := true;
        break;
      end;
    end;
  end;
  if (collision) then dispose(newMapPiece, destroy) else
  begin
    placedPieces.add(newMapPiece);
    newMapPieceEnum := new(pmapPieceEnum);
    newMapPieceEnum^ := currentMapPiece;
    placedPieceEnums.add(newMapPieceEnum);

    repeat
      newMapPiece^.rotate(0, rotationToMouse(newMapPiece), 0);

      setupEditorMatrices;
      drawFloor;

      tintShader^.bind;
      tintShader^.use;
      tintShader^.setUniform4(EXTRA1_LOCATION, 1.0, 1.0, 1.0, 1.0);

      i := 0;
      collision := false;
      while (i < placedPieces.count) do
      begin
        tempGameActor := PGameActor(placedPieces[i]);
        tempMapPieceEnum := pmapPieceEnum(placedPieceEnums[i]);
        if ((tempMapPieceEnum^ = CAT_SPAWNPOINT) or (tempMapPieceEnum^ = SPAMBOT_SPAWNPOINT)
           or (tempMapPieceEnum^ = MECH_SPAWNPOINT)) then
        begin
          tintShader^.use;
          if (tempMapPieceEnum^ = CAT_SPAWNPOINT) then tintShader^.setUniform4(EXTRA1_LOCATION, 0.4, 0.4, 1.0, 1.0)
             else tintShader^.setUniform4(EXTRA1_LOCATION, 1.0, 0.0, 0.0, 1.0);
          tempGameActor^.draw(true, true);
          tintShader^.use;
          tintShader^.setUniform4(EXTRA1_LOCATION, 1.0, 1.0, 1.0, 1.0);
        end else tempGameActor^.draw(true, true);

        i += 1;
      end;


      refreshScreen;
    until mouseLeft;
  end;
end;

procedure deleteMapPiece;
  function mouseCollision(actor : PGameActor) : boolean;
  var
    xDist, zDist : real;
  begin
    result := false;

    xDist := mouseX-xPos-actor^.x;
    if (abs(xDist) > actor^.getRadius) then exit;
    zDist := -mouseY+zPos-actor^.z;
    if (abs(zDist) > actor^.getRadius) then exit;

    result := true;
  end;

var
  tempGameActor : PGameActor;
  i : integer;
begin
  for i := 0 to placedPieces.count-1 do
  begin
    tempGameActor := PGameActor(placedPieces[i]);
    if (mouseCollision(tempGameActor)) then tempGameActor^.dieFlag := true;
  end;
end;

procedure newGhostPiece;
begin
  if (ghostPiece <> nil) then dispose(ghostPiece, destroy);

  case currentMapPiece of
  ROCK : ghostPiece := new(PRock, create(mouseX-xPos, -mouseY+zPos, 0));
  TREE : ghostPiece := new(PTree, create(mouseX-xPos, -mouseY+zPos, 0));
  CRATE : ghostPiece := new(PCrate, create(mouseX-xPos, -mouseY+zPos, 0));
  HOUSE : ghostPiece := new(PHouse, create(mouseX-xPos, -mouseY+zPos, 0));
  BUNGALOW : ghostPiece := new(PBungalow, create(mouseX-xPos, -mouseY+zPos, 0));
  BAR : ghostPiece := new(PBar, create(mouseX-xPos, -mouseY+zPos, 0));
  SKYSCRAPER : ghostPiece := new(PSkyScraper, create(mouseX-xPos, -mouseY+zPos, 0));
  STATUE : ghostPiece := new(PStatue, create(mouseX-xPos, -mouseY+zPos, 0));
  SACK_OF_CATNIP : ghostPiece := new(PSack, create(mouseX-xPos, -mouseY+zPos, 0));
  CAT_SPAWNPOINT : ghostPiece := new(PCatSpawnPoint, create(mouseX-xPos, -mouseY+zPos, 0));
  SPAMBOT_SPAWNPOINT : ghostPiece := new(PSpambotSpawnPoint, create(mouseX-xPos, -mouseY+zPos, 0));
  MECH_SPAWNPOINT : ghostPiece := new(PMechSpawnPoint, create(mouseX-xPos, -mouseY+zPos, 0));
  end;
end;

procedure incrementMapPiece;
begin
  if (currentMapPiece = LAST_PIECE) then currentMapPiece := FIRST_PIECE else inc(currentMapPiece);
  newGhostPiece;
end;

procedure decrementMapPiece;
begin
  if (currentMapPiece = FIRST_PIECE) then currentMapPiece := LAST_PIECE else dec(currentMapPiece);
  newGhostPiece;
end;

procedure writeDetails;
begin
  clrScr;
  gotoXY(2, 2);
  write('Current map piece: ', currentMapPiece);
  gotoXY(2, 5);
  write('X position: ', round(xPos));
  gotoXY(2, 6);
  write('Y position: ', round(zPos));
  gotoXY(2, 8);
end;

procedure disposeOfMapPieces;
var
  i : integer;
  tempGameActor : PGameActor;
  tempMapPieceEnum : pmapPieceEnum;
begin
  if (placedPieces.count > 0) then
  begin
    for i := 0 to placedPieces.count-1 do
    begin
      tempGameActor := PGameActor(placedPieces[i]);
      dispose(tempGameActor, destroy);
      tempMapPieceEnum := pmapPieceEnum(placedPieceEnums[i]);
      dispose(tempMapPieceEnum);
    end;
  end;
  placedPieces.clear;
  placedPieceEnums.clear;
end;

procedure loadMap;
var
  mapToLoad, tempStr : string;
  mapFile : text;
  mapPieceToLoad : mapPieceEnum;
  mapPieceX, mapPieceZ, mapPieceRotation : integer;
  newMapPieceEnum : pmapPieceEnum;
begin
  clrscr;
  gotoXY(1, 1);
  writeln('What map would you like to load?');
  readln(mapToLoad);

  if (directoryExists('assets/maps')) then
  begin
    if (rightStr(mapToLoad, 4) <> '.map') then mapToLoad += '.map';
    if (fileExists('assets/maps/'+mapToLoad)) then
    begin
      disposeOfMapPieces;

      assign(mapFile, 'assets/maps/'+mapToLoad);
      reset(mapFile);

      readln(mapFile, tempStr);
      writeln('Loading map '''+tempStr+'''...');

      repeat
        readln(mapFile, tempStr);
        mapPieceToLoad := mapPieceEnum(GetEnumValue(TypeInfo(mapPieceEnum), tempStr));

        readln(mapFile, tempStr);
        mapPieceX := strToInt(tempStr);
        readln(mapFile, tempStr);
        mapPieceZ := strToInt(tempStr);
        readln(mapFile, tempStr);
        mapPieceRotation := strToInt(tempStr);

        case mapPieceToLoad of
        ROCK : placedPieces.add(new(PRock, create(mapPieceX, mapPieceZ, mapPieceRotation)));
        TREE : placedPieces.add(new(PTree, create(mapPieceX, mapPieceZ, mapPieceRotation)));
        CRATE : placedPieces.add(new(PCrate, create(mapPieceX, mapPieceZ, mapPieceRotation)));
        HOUSE : placedPieces.add(new(PHouse, create(mapPieceX, mapPieceZ, mapPieceRotation)));
        BUNGALOW : placedPieces.add(new(PBungalow, create(mapPieceX, mapPieceZ, mapPieceRotation)));
        BAR : placedPieces.add(new(PBar, create(mapPieceX, mapPieceZ, mapPieceRotation)));
        SKYSCRAPER : placedPieces.add(new(PSkyScraper, create(mapPieceX, mapPieceZ, mapPieceRotation)));
        STATUE : placedPieces.add(new(PStatue, create(mapPieceX, mapPieceZ, mapPieceRotation)));
        SACK_OF_CATNIP : placedPieces.add(new(PSack, create(mapPieceX, mapPieceZ, mapPieceRotation)));
        CAT_SPAWNPOINT : placedPieces.add(new(PCatSpawnPoint, create(mapPieceX, mapPieceZ, mapPieceRotation)));
        SPAMBOT_SPAWNPOINT : placedPieces.add(new(PSpambotSpawnPoint, create(mapPieceX, mapPieceZ, mapPieceRotation)));
        MECH_SPAWNPOINT : placedPieces.add(new(PMechSpawnPoint, create(mapPieceX, mapPieceZ, mapPieceRotation)));
        end;
        newMapPieceEnum := new(pmapPieceEnum);
        newMapPieceEnum^ := mapPieceToLoad;
        placedPieceEnums.add(newMapPieceEnum);
      until eof(mapFile);

      close(mapFile);
      writeln('File assets/maps/'+mapToLoad+' loaded');
      wait(400);
    end else writeln('Map '+mapToLoad+' does not exist');
  end else writeln('No maps to load');
end;

procedure saveMap;
var
  mapName, response : string;
  mapFile : text;
  i : integer;
  tempGameActor : PGameActor;
  tempMapPieceEnum : pmapPieceEnum;
begin
  clrscr;
  gotoXY(1, 1);
  writeln('What would you like to call the map?');
  readln(mapName);
  if (not directoryExists('assets/maps')) then mkdir('assets/maps');

  if (fileExists('assets/maps/'+mapName+'.map')) then
  begin
    writeln('The file assets/maps/'+mapName+'.map already exists.');
    writeln('Would you like to replace the file? Type Y/y to rewrite, N/n to change the name of the new map, or anything else to cancel');
    readln(response);
    if (lowerCase(response) <> 'y') then
    begin
      if (lowerCase(response) = 'n') then saveMap;
      exit;
    end;
  end;

  assign(mapFile, 'assets/maps/'+mapName+'.map');
  rewrite(mapFile);

  writeln(mapFile, mapName);
  for i := 0 to placedPieces.count-1 do
  begin
    tempMapPieceEnum := pmapPieceEnum(placedPieceEnums[i]);
    writeln(mapFile, tempMapPieceEnum^);
    tempGameActor := PGameActor(placedPieces[i]);
    writeln(mapFile, round(tempGameActor^.x));
    writeln(mapFile, round(tempGameActor^.z));
    writeln(mapFile, round(tempGameActor^.yRotation));
  end;

  close(mapFile);

  writeln('Map saved to assets/maps/'+mapName+'.map');
  wait(400);
end;

procedure runLevelEditor;
var
  i : integer;
  tempGameActor : PGameActor;
  tempMapPieceEnum : pmapPieceEnum;
  collision : boolean;
begin
  showCursor;
  placedPieces := TList.create;
  placedPieceEnums := TList.create;
  currentMapPiece := FIRST_PIECE;
  newGhostPiece;
  xPos := 0;
  zPos := 0;

  repeat
    if (mouseLeft) then
    begin
      placeMapPiece;
      wait(200);
    end;
    if (mouseRight) then
    begin
      deleteMapPiece;
      wait(200);
    end;
    if (keyPressed(ord('l'))) then
    begin
      incrementMapPiece;
      wait(200);
    end;
    if (keyPressed(ord('j'))) then
    begin
      decrementMapPiece;
      wait(200);
    end;
    if (keyPressed(ord('1'))) then saveMap;
    if (keyPressed(ord('`'))) then loadMap;

    if (keyPressed(ord('w'))) then zPos += compensation*5;
    if (keyPressed(ord('s'))) then zPos -= compensation*5;
    if (keyPressed(ord('a'))) then xPos += compensation*5;
    if (keyPressed(ord('d'))) then xPos -= compensation*5;

    setupEditorMatrices;
    drawFloor;

    tintShader^.bind;
    tintShader^.use;
    tintShader^.setUniform4(EXTRA1_LOCATION, 1.0, 1.0, 1.0, 1.0);

    i := 0;
    collision := false;
    while (i < placedPieces.count) do
    begin
      tempGameActor := PGameActor(placedPieces[i]);
      if (tempGameActor^.dieFlag) then
      begin
        dispose(tempGameActor, destroy);
        placedPieces.delete(i);
        tempMapPieceEnum := pmapPieceEnum(placedPieceEnums[i]);
        dispose(tempMapPieceEnum);
        placedPieceEnums.delete(i);
        continue;
      end;

      tempMapPieceEnum := pmapPieceEnum(placedPieceEnums[i]);
      if ((tempMapPieceEnum^ = CAT_SPAWNPOINT) or (tempMapPieceEnum^ = SPAMBOT_SPAWNPOINT)
         or (tempMapPieceEnum^ = MECH_SPAWNPOINT)) then
      begin
        tintShader^.use;
        if (tempMapPieceEnum^ = CAT_SPAWNPOINT) then tintShader^.setUniform4(EXTRA1_LOCATION, 0.4, 0.4, 1.0, 1.0)
           else tintShader^.setUniform4(EXTRA1_LOCATION, 1.0, 0.0, 0.0, 1.0);
        tempGameActor^.draw(true, true);
        tintShader^.use;
        tintShader^.setUniform4(EXTRA1_LOCATION, 1.0, 1.0, 1.0, 1.0);
      end else tempGameActor^.draw(true, true);

      if (ghostPiece^.collision(tempGameActor)) then collision := true;

      i += 1;
    end;

    tintShader^.use;
    if (collision) then tintShader^.setUniform4(EXTRA1_LOCATION, 1.0, 0.0, 0.0, 0.4)
       else tintShader^.setUniform4(EXTRA1_LOCATION, 1.0, 1.0, 1.0, 0.4);
    ghostPiece^.setPosition(mouseX-xPos, ghostPiece^.y, -mouseY+zPos);
    ghostPiece^.draw(true, true);

    writeDetails;

    refreshScreen;
  until keyPressed(27);

  disposeOfMapPieces;
  placedPieces.destroy;
  placedPieceEnums.destroy;
  dispose(ghostPiece, destroy);
  clrscr;
end;

end.

