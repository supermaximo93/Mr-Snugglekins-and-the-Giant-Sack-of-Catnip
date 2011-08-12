unit TurretClass;

{$mode objfpc}{$H+}

interface

uses GameActorClass;

type
  PTurret = ^TTurret;
  TTurret = object(TGameActor)
  protected
    fireLevel, rangeLevel, healthLevel : integer;
    initialRotation, rateOfFire, lastShot, targetDistance, timeFromDamage, hackAmount, timeFromHack, timeFromHackCompletion, timeFromSmoke : real;
    shooting, rotationFlag, hacked, beingHacked, hackedThisFrame : boolean;
    target : PGameActor;
  public
    destructor destroy; virtual;
    procedure init;
    procedure pausedDraw; virtual;
    procedure fireLevelUp;
    procedure rangeLevelUp;
    procedure healthLevelUp;
    procedure update; virtual;
    procedure shoot; virtual; abstract;
    function getTarget : PGameActor;
    procedure setTarget(newTarget : PGameActor);
    procedure hack;
    function isHacked : boolean;
    function readyToUpgrade : boolean;
    function getFireLevel : integer;
    function getRangeLevel : integer;
    function getHealthLevel : integer;
  end;

  PKittyTurret = ^TKittyTurret;
  TKittyTurret = object(TTurret)
  public
    constructor create(destX, destZ, newRotation : real; alreadyHacked : boolean = false);
    procedure shoot; virtual;
  end;

  PAkatTurret = ^TAkatTurret;
  TAkatTurret = object(TTurret)
  public
    constructor create(destX, destZ, newRotation : real; alreadyHacked : boolean = false);
    procedure update; virtual;
    procedure shoot; virtual;
  end;

procedure initTurrets;
procedure freeTurrets;

procedure turretUpgradeMenu(turret : PTurret);

implementation

uses SysUtils, Math, Display, SoundClass, GraphicalAssetClasses, ShaderClass, GlobalGameVariables, BulletClass, ExplosionClass, FloatScoreClass, SpambotClass;

var
  kittyTurretModel, akatTurretModel : PModel;
  turretUpgradeSegment, turretUpgradeSegmentOverlay : PSprite;
  hackedSound, whooshSound : PSound;

procedure initTurrets;
begin
  kittyTurretModel := new(PModel, create('turret', 'assets/models/kittyturret/', 'kittyturret.smo'));
  akatTurretModel := new(PModel, create('turret', 'assets/models/akatturret/', 'akatturret.smo'));

  turretUpgradeSegment := new(PSprite, create('turretUpgradeSegment', 'assets/sprites/upgrade_segment.png', 1, 1, 150, 174, 1, 1, 0, 86));
  turretUpgradeSegment^.bindShader(fontShader);
  turretUpgradeSegmentOverlay := new(PSprite, create('turretUpgradeSegmentOverlay', 'assets/sprites/upgrade_segment_overlay.png', 1, 1, 150, 174, 1, 1, 0, 86));
  turretUpgradeSegmentOverlay^.bindShader(spriteShader);

  hackedSound := new(PSound, create('hacked', 'assets/sounds/beep-09.ogg'));
  whooshSound := new(PSound, create('whoosh', 'assets/sounds/wind-02.ogg'));
end;

procedure freeTurrets;
begin
  dispose(whooshSound, destroy);
  dispose(hackedSound, destroy);
  dispose(turretUpgradeSegmentOverlay, destroy);
  dispose(turretUpgradeSegment, destroy);
  dispose(akatTurretModel, destroy);
  dispose(kittyTurretModel, destroy);
end;

procedure turretUpgradeMenu(turret : PTurret);
const
  FIRE_UPGRADE_ID = 0;
  RANGE_UPGRADE_ID = 1;
  HEALTH_UPGRADE_ID = 2;
  HEALTH_REPLENISH_ID = 3;
  EXIT_ID = 4;
  UPGRADE_TEXT : array[0..EXIT_ID] of string = ('Firepower upgrade', 'Range upgrade', 'Health upgrade', 'Heal 100 health', 'Exit');
  UPGRADE_CHAR : array[0..EXIT_ID] of char = ('l', '''', 'y', 'k', '*');
var
  upgradeId, i : integer;
  dist : array[0..EXIT_ID] of real = (180, 180, 180, 180, 180);
  destDist : array [0..EXIT_ID] of real = (0, 0, 0, 0, 0);
  stop : boolean = false;
  r, g, b, tempXDistance, tempZDistance, tempRot, tempCatRot, tempCatX, tempCatZ, xDiff, zDiff, rot, timeFromSelect : real;
  tempStr : string;
begin
  tempXDistance := xDistance;
  tempZDistance := zDistance;
  tempRot := 0;

  cat^.setModel(catWithSpannerModel);
  cat^.setFrame(0);
  cat^.gun^.setVisibility(false);
  tempCatRot := cat^.yRotation;
  tempCatX := cat^.x;
  tempCatZ := cat^.z;

  xDiff := cat^.x-turret^.x;
  if (xDiff = 0) then xDiff := 1;
  zDiff := cat^.z-turret^.z;
  rot := radToDeg(arcTan(zDiff/xDiff));
  if (rot < 0) then rot := -rot-90 else rot := -rot+90;
  if (zDiff > 0) then rot := rot+180;
  cat^.rotate(cat^.xRotation, rot, cat^.zRotation);
  cat^.setX(turret^.x-sin(degTorad(rot))*18);
  cat^.setZ(turret^.z-cos(degToRad(rot))*18);

  whooshSound^.play(round(30*soundEffectsVolume));

  repeat
    if ((xDistance > -turret^.x-compensation*2) and (xDistance < -turret^.x+compensation*2)) then xDistance := -turret^.x else
    begin
      if (xDistance < -turret^.x) then xDistance += compensation*0.8 else xDistance -= compensation*0.8;
    end;

    if ((zDistance > -turret^.z-compensation*2) and (zDistance < -turret^.z+compensation*2)) then zDistance := -turret^.z else
    begin
      if (zDistance < -turret^.z) then zDistance += compensation*0.8 else zDistance -= compensation*0.8;
    end;

    if (yDistance > 15) then yDistance := 15 else yDistance += compensation;

    rotation -= tempRot;

    if (tempRot > 180-compensation*8) then tempRot := 180 else tempRot += compensation*8;

    rotation += tempRot;

    drawStill;
    drawSprites;
    drawText;
    setup2dMatrices;
    refreshScreen;
  until ((xDistance = -turret^.x) and (zDistance = -turret^.z) and (yDistance = 15) and (tempRot = 180));

  upgradeId := 0;
  timeFromSelect := 21;

  repeat
    cat^.setFrame(1, true);
    drawStill;
    drawSprites;
    setup2dMatrices;

    if ((right or up) and (dist[upgradeId] = destDist[upgradeId])) then
    begin
      upgradeId += 1;
      if (upgradeId > EXIT_ID) then upgradeId := 0;
    end else if ((left or down) and (dist[upgradeId] = destDist[upgradeId])) then
    begin
      upgradeId -= 1;
      if (upgradeId < 0) then upgradeId := EXIT_ID;
    end;

    if (trigger and (dist[upgradeId] = destDist[upgradeId]) and (timeFromSelect > 20)) then
    begin
      case upgradeId of
      FIRE_UPGRADE_ID :
        begin
          if ((money >= FIRE_UPGRADE_PRICE*turret^.fireLevel) and (turret^.fireLevel < TURRET_MAX_LEVEL)) then
          begin
            money -= FIRE_UPGRADE_PRICE;
            turret^.fireLevelUp;
            cashRegisterSound^.play(round(40*soundEffectsVolume));
          end;
        end;
      RANGE_UPGRADE_ID :
        begin
          if ((money >= RANGE_UPGRADE_PRICE*turret^.rangeLevel) and (turret^.rangeLevel < TURRET_MAX_LEVEL)) then
          begin
            money -= RANGE_UPGRADE_PRICE;
            turret^.rangeLevelUp;
            cashRegisterSound^.play(round(40*soundEffectsVolume));
          end;
        end;
      HEALTH_UPGRADE_ID :
        begin
          if ((money >= HEALTH_UPGRADE_PRICE*turret^.healthLevel) and (turret^.healthLevel < TURRET_MAX_LEVEL)) then
          begin
            money -= HEALTH_UPGRADE_PRICE;
            turret^.healthLevelUp;
            cashRegisterSound^.play(round(40*soundEffectsVolume));
          end;
        end;
      HEALTH_REPLENISH_ID :
        begin
          if ((money >= HEALTH_REPLENISH_PRICE) and (turret^.getHealth < turret^.getInitialHealth)) then
          begin
            money -= HEALTH_REPLENISH_PRICE;
            turret^.heal(TURRET_HEALTH_REPLENISH_AMOUNT);
          end;
        end;
      EXIT_ID : stop := true;
      end;

      timeFromSelect := 0
    end;

    for i := 0 to EXIT_ID do
    begin
      destDist[i] := 180;
      r := 0.5;
      g := 0.5;
      b := 0.5;

      case i of
      FIRE_UPGRADE_ID :
        begin
          if ((money >= FIRE_UPGRADE_PRICE*turret^.fireLevel) and (turret^.fireLevel < TURRET_MAX_LEVEL)) then
          begin
            r := 1.0;
            g := 0.5;
            b := 0.0;
          end;
          tempStr := intToStr(turret^.getFireLevel);
          if (upgradeId = FIRE_UPGRADE_ID) then destDist[i] := 210;
          tenneryBold^.write(tempStr, round((screenWidth/2)+sin(degToRad(i*72+252))*dist[i]-(tenneryBold^.width(tempStr)/2)),
            round((screenHeight/2)-cos(degToRad(i*72+252))*dist[i]-(tenneryBold^.height(tempStr)/2)), -1);
        end;
      RANGE_UPGRADE_ID :
        begin
          if ((money >= RANGE_UPGRADE_PRICE*turret^.rangeLevel) and (turret^.rangeLevel < TURRET_MAX_LEVEL)) then
          begin
            r := 0.0;
            g := 0.0;
            b := 1.0;
          end;
          tempStr := intToStr(turret^.getRangeLevel);
          if (upgradeId = RANGE_UPGRADE_ID) then destDist[i] := 210;
          tenneryBold^.write(tempStr, round((screenWidth/2)+sin(degToRad(i*72+252))*dist[i]-(tenneryBold^.width(tempStr)/2)),
            round((screenHeight/2)-cos(degToRad(i*72+252))*dist[i]-(tenneryBold^.height(tempStr)/2)), -1);
        end;
      HEALTH_UPGRADE_ID :
        begin
          if ((money >= HEALTH_UPGRADE_PRICE*turret^.healthLevel) and (turret^.healthLevel < TURRET_MAX_LEVEL)) then
          begin
            r := 0.0;
            g := 1.0;
            b := 0.0;
          end;
          tempStr := intToStr(turret^.getHealthLevel);
          if (upgradeId = HEALTH_UPGRADE_ID) then destDist[i] := 210;
          tenneryBold^.write(tempStr, round((screenWidth/2)+sin(degToRad(i*72+252))*dist[i]-(tenneryBold^.width(tempStr)/2)),
            round((screenHeight/2)-cos(degToRad(i*72+252))*dist[i]-(tenneryBold^.height(tempStr)/2)), -1);
        end;
      HEALTH_REPLENISH_ID :
        begin
          if ((money >= HEALTH_REPLENISH_PRICE) and (turret^.getHealth < turret^.getInitialHealth)) then
          begin
            r := 1.0;
            g := 0.0;
            b := 0.0;
          end;
          tempStr := intToStr(round(turret^.getHealth));
          if (upgradeId = HEALTH_REPLENISH_ID) then destDist[i] := 210;
          tenneryBold^.write(tempStr, round((screenWidth/2)+sin(degToRad(i*72+252))*dist[i]-(tenneryBold^.width(tempStr[1])/2)),
            round((screenHeight/2)-cos(degToRad(i*72+252))*dist[i]-(tenneryBold^.height(tempStr[1])/2)), -1);
        end;
      EXIT_ID :
        begin
          r := 0.2;
          g := 0.2;
          b := 0.2;
          if (upgradeId = EXIT_ID) then destDist[i] := 210;
        end;
      end;

      if ((dist[i] < destDist[i]+compensation*3) and (dist[i] > destDist[i]-compensation*3)) then dist[i] := destDist[i] else
      begin
        if (dist[i] < destDist[i]) then dist[i] += compensation*3 else if (dist[i] > destDist[i]) then dist[i] -= compensation*3;
      end;

      fontShader^.use;
      fontShader^.setUniform4(EXTRA0_LOCATION, r, g, b, 1.0);

      turretUpgradeSegment^.draw(round((screenWidth/2)+sin(degToRad(i*72+252))*(dist[i]-180)), round((screenHeight/2)-cos(degToRad(i*72+252))*(dist[i]-180)), -1, i*72+162);
      fontShader^.use;
      fontShader^.setUniform4(EXTRA0_LOCATION, 1.0, 1.0, 1.0, 1.0);
      pictosWeb^.write(UPGRADE_CHAR[i], round((screenWidth/2)+sin(degToRad(i*72+252))*(dist[i]-60)-(pictosWeb^.width(UPGRADE_CHAR[i])/2)),
        round((screenHeight/2)-cos(degToRad(i*72+252))*(dist[i]-60)-(pictosWeb^.height(UPGRADE_CHAR[i])/2)), -1);
      turretUpgradeSegmentOverlay^.draw(round((screenWidth/2)+sin(degToRad(i*72+252))*(dist[i]-180)), round((screenHeight/2)-cos(degToRad(i*72+252))*(dist[i]-180)), -1, i*72+162);
    end;

    tempStr := '$';
    case upgradeId of
    FIRE_UPGRADE_ID : if (turret^.fireLevel < TURRET_MAX_LEVEL) then tempStr += intToStr(FIRE_UPGRADE_PRICE*turret^.fireLevel) else tempStr := 'X';
    RANGE_UPGRADE_ID : if (turret^.rangeLevel < TURRET_MAX_LEVEL) then tempStr += intToStr(RANGE_UPGRADE_PRICE*turret^.rangeLevel) else tempStr := 'X';
    HEALTH_UPGRADE_ID : if (turret^.healthLevel < TURRET_MAX_LEVEL) then tempStr += intToStr(HEALTH_UPGRADE_PRICE*turret^.healthLevel) else tempStr := 'X';
    HEALTH_REPLENISH_ID : if (turret^.getHealth < turret^.getInitialHealth) then tempStr += intToStr(HEALTH_REPLENISH_PRICE) else tempStr := 'X';
    EXIT_ID : tempStr := ' ';
    end;
    tenneryBold^.write(tempStr, round((screenWidth/2)-(tenneryBold^.width(tempStr)/2)), round((screenHeight/2)-(tenneryBold^.height(tempStr)/2)), -1, false);
    tenneryBold^.write(UPGRADE_TEXT[upgradeId], round((screenWidth/2)-(tenneryBold^.width(UPGRADE_TEXT[upgradeId])/2)), round(screenHeight-(tenneryBold^.height(UPGRADE_TEXT[upgradeId])/2))-40, -1);

    drawText;

    fontShader^.use;
    fontShader^.setUniform4(EXTRA0_LOCATION, 0.0, 0.0, 0.0, 1.0);
    tenneryBold^.write('You have $'+intToStr(money), screenWidth-20-tenneryBold^.width('You have $'+intToStr(money)), 20, -1);
    fontShader^.use;
    fontShader^.setUniform4(EXTRA0_LOCATION, 1.0, 1.0, 1.0, 1.0);

    refreshScreen;
    timeFromSelect += compensation;
  until stop;

  setup3dMatrices;
  tintShader^.bind;
  tintShader^.use;
  tintShader^.setUniform4(EXTRA1_LOCATION, 1.0, 1.0, 1.0, 1.0);

  repeat
    if ((xDistance < tempXDistance+compensation*2) and (xDistance > tempXDistance-compensation*2)) then xDistance := tempXDistance else
    begin
      if (xDistance < tempXDistance) then xDistance += compensation*0.8 else xDistance -= compensation*0.8;
    end;

    if ((zDistance < tempZDistance+compensation*2) and (zDistance > tempZDistance-compensation*2)) then zDistance := tempZDistance else
    begin
      if (zDistance < tempZDistance) then zDistance += compensation*0.8 else zDistance -= compensation*0.8;
    end;

    if (yDistance < 0) then yDistance := 0 else yDistance -= compensation;

    rotation -= tempRot;

    if (tempRot < compensation*8) then tempRot := 0 else tempRot -= compensation*8;

    rotation += tempRot;

    drawStill;
    drawSprites;
    drawText;
    setup2dMatrices;
    refreshScreen;
  until ((xDistance = tempXDistance) and (zDistance = tempZDistance) and (yDistance = 0) and (tempRot = 0));
  cat^.setModel(catModel);
  cat^.gun^.setVisibility(true);
  cat^.rotate(cat^.xRotation, tempCatRot, cat^.zRotation);
  cat^.setX(tempCatX);
  cat^.setZ(tempCatZ);
end;

destructor TTurret.destroy;
var
  i : integer;
  tempSpambot : PSpambot;
begin
  collidables.delete(collidables.indexOf(@self));
  turrets.delete(turrets.indexOf(@self));

  if (not quit) then
  begin
    if (spambots.count > 0) then
    begin
      for i := 0 to spambots.count-1 do
      begin
        tempSpambot := PSpambot(spambots[i]);
        if (tempSpambot^.getObstacle = @self) then tempSpambot^.setObstacle(nil);
        if (tempSpambot^.getTarget = @self) then tempSpambot^.resetAttack;
      end;
    end;
  end;
  TGameObject.destroy;
end;

procedure TTurret.init;
var
  i : integer;
  tempSpambot : PSpambot;
begin
  dieFlag := false;
  radius := 15;
  fireLevel := 1;
  rangeLevel := 1;
  healthLevel := 1;
  lastShot := 0;
  shooting := false;
  rotationFlag := false;
  target := nil;
  targetDistance := 0;
  initialHealth := TURRET_HEALTH;
  health := initialHealth;
  attributes := [];
  hackAmount := 0;
  timeFromHack := 0;
  timeFromHackCompletion := 0;
  timeFromSmoke := 0;

  if (spambots.count > 0) then
  begin
    for i := 0 to spambots.count-1 do
    begin
      tempSpambot := PSpambot(spambots[i]);
      if (tempSpambot^.getType = HACKER) then
      begin
        tempSpambot^.resetAttack;
        PHacker(tempSpambot)^.updateHackTarget;
      end;
    end;
  end;

  collidables.add(@self);
  turrets.add(@self);
end;

procedure TTurret.pausedDraw;
var
  shaderToUse : PShader;
begin
  if (health < initialHealth/3) then
  begin
    shaderToUse := tintAndWearShader;
    tintAndWearShader^.use;
    tintAndWearShader^.setUniform4(EXTRA1_LOCATION, 1.0, 1.0, 1.0, 1.0);
    tintAndWearShader^.setUniform1(EXTRA2_LOCATION, 1);
    tintAndWearShader^.setUniform1(EXTRA3_LOCATION, 0);
    bindDissolveMap;
  end else shaderToUse := tintShader;

  shaderToUse^.use;
  if (justDamaged) then shaderToUse^.setUniform4(EXTRA1_LOCATION, 1.0, 0.2, 0.2, 1.0) else
  begin
    if (hacked) then shaderToUse^.setUniform4(EXTRA1_LOCATION, 1.0, 0.0+(hackAmount/TURRET_HACK_TIME), 1.0, 1.0) else shaderToUse^.setUniform4(EXTRA1_LOCATION, 1.0, 1.0-(hackAmount/TURRET_HACK_TIME), 1.0, 1.0);
  end;


  shaderToUse^.bind;
  draw(false, true);
  tintShader^.bind;
  tintShader^.use;
  tintShader^.setUniform4(EXTRA1_LOCATION, 1.0, 1.0, 1.0, 1.0);
end;

procedure TTurret.fireLevelUp;
begin
  if (fireLevel < TURRET_MAX_LEVEL) then fireLevel += 1;
end;

procedure TTurret.rangeLevelUp;
begin
  if (rangeLevel < TURRET_MAX_LEVEL) then rangeLevel += 1;
end;

procedure TTurret.healthLevelUp;
begin
  if (healthLevel < TURRET_MAX_LEVEL) then
  begin
    healthLevel += 1;
    initialHealth += 100;
    health += 100;
    if (health > initialHealth) then health := initialHealth;
  end;
end;

function TTurret.getFireLevel : integer;
begin
  result := fireLevel;
end;

function TTurret.getRangeLevel : integer;
begin
  result := rangeLevel;
end;

function TTurret.getHealthLevel : integer;
begin
  result := healthLevel;
end;

procedure TTurret.update;
  procedure reset;
  var
    diff1, diff2 : real;
  begin
    if (yRotation_ < 0) then yRotation_ += 360 else if (yRotation_ >= 360) then yRotation_ -= 360;
    diff1 := initialRotation+45-yRotation_;
    diff2 := initialRotation+315-yRotation_;
    if (abs(diff1) <= abs(diff2)) then
    begin
      rotationFlag := not (abs(diff1) > initialRotation+45);
    end
  else
    begin
      rotationFlag := (abs(diff2) > initialRotation+45);
    end;
  end;
var
  zDiff, xDiff, rot, tempInitialRotation1, tempInitialRotation2 : real;
  tempTarget : PGameActor = nil;
  shaderToUse : PShader;
begin
  shaderToUse := tintShader;

  if (health < 1) then
  begin
    dieFlag := true;

    gameActors.add(new(PExplosion, create(x_, y_+12, z_, 0.5)));
    if (hacked) then
    begin
      gameActors.add(new(PFloatScore, create(TURRET_REWARD, x_, y_+20, z_)));
      turretKills[ENEMY_TURRET] += 1;
    end
  else
    begin
      gameActors.add(new(PFloatText, create('-'+intToStr(TURRET_REWARD)+' pts', x_, y_+20, z_, tenneryBold, 1.0, 0.0, 0.0)));
      score -= TURRET_REWARD;
      turretKills[FRIENDLY_TURRET] += 1;
    end;
    exit;
  end else if (health < (initialHealth/3)*2) then
  begin
    timeFromSmoke += compensation;
    if (timeFromSmoke > 150) then
    begin
      timeFromSmoke := 0;
      gameActors.add(new(PFloatSprite, create(smoke, x_, 8, z_, 0.3)));
    end;
    if (health < initialHealth/3) then
    begin
      if (visible_) then
      begin
        shaderToUse := tintAndWearShader;
        tintAndWearShader^.use;
        tintAndWearShader^.setUniform4(EXTRA1_LOCATION, 1.0, 1.0, 1.0, 1.0);
        tintAndWearShader^.setUniform1(EXTRA2_LOCATION, 1);
        tintAndWearShader^.setUniform1(EXTRA3_LOCATION, 0);
        bindDissolveMap;
      end;
    end;
  end;

  if (beingHacked) then
  begin
    target := nil;
    lastShot := rateOfFire;
    timeFromHack += compensation;
    if (timeFromHack > 60) then beingHacked := false;
  end
else
  begin
    if (shooting) then
    begin
      if (target = nil) then
      begin
        shooting := false;
        reset;
      end
    else
      begin
        targetDistance := hitScan(x_-(sin(degToRad(-yRotation_))*20), z_+(cos(degToRad(-yRotation_))*20), -yRotation_+180, TURRET_RANGE*rangeLevel, tempTarget);
        if (tempTarget <> target) then
        begin
          shooting := false;
          reset;
        end
      else
        begin
          xDiff := x_-target^.x;
          if (xDiff = 0) then xDiff := 1;
          zDiff := z_-target^.z;
          if (ZDiff = 0) then zDiff := 1;
          rot := radToDeg(arcTan(zDiff/xDiff));
          if (rot < 0) then rot := -rot-90 else rot := -rot+90;
          if (zDiff > 0) then rot := rot+180;
          yRotation_ := rot;
          if (lastShot >= rateOfFire) then
          begin
            lastShot := 0;
            shoot;
          end;
          setFrame((5/rateOfFire)*fireLevel, true);
        end;
      end;
    end
  else
    begin
      setFrame(0);
      if (rotationFlag) then yRotation_ += TURRET_ROTATION_SPEED*compensation else yRotation_ -= TURRET_ROTATION_SPEED*compensation;
      if (yRotation_ < 0) then yRotation_ += 360 else if (yRotation_ >= 360) then yRotation_ -= 360;
      tempInitialRotation1 := initialRotation;
      tempInitialRotation2 := initialRotation;
      if (initialRotation+45 >= 360) then tempInitialRotation1 -= 360;
      if (initialRotation+315 >= 360) then tempInitialRotation2 -= 360;
      if ((yRotation_ > tempInitialRotation1+45) and (yRotation_ <= tempInitialRotation1+180)) then rotationFlag := false
        else if ((yRotation_ < tempInitialRotation2+315) and (yRotation_ > tempInitialRotation2+180)) then rotationFlag := true
    else
      begin
        if (hacked) then
        begin
          if (cat <> nil) then
          begin
            hitScan(x_-(sin(degToRad(-yRotation_))*20), z_+(cos(degToRad(-yRotation_))*20), -yRotation_+180, TURRET_RANGE*rangeLevel, target);
            if (target = PGameActor(cat)) then shooting := true;
          end;
        end
      else
        begin
          hitScan(x_-(sin(degToRad(-yRotation_))*20), z_+(cos(degToRad(-yRotation_))*20), -yRotation_+180, TURRET_RANGE*rangeLevel, target, true);
          if (target <> nil) then shooting := true;
        end;
      end;
    end;
    if (lastShot < rateOfFire) then lastShot += compensation;
  end;

  shaderToUse^.use;
  if (justDamaged) then
  begin
    if (timeFromDamage > DAMAGE_TINT_TIME) then
    begin
      justDamaged := false;
      timeFromDamage := 0;
    end else timeFromDamage += compensation;
    shaderToUse^.setUniform4(EXTRA1_LOCATION, 1.0, 0.2, 0.2, 1.0);
  end
else
  begin
    if (hacked) then shaderToUse^.setUniform4(EXTRA1_LOCATION, 1.0, 0.0+(hackAmount/TURRET_HACK_TIME), 1.0, 1.0) else shaderToUse^.setUniform4(EXTRA1_LOCATION, 1.0, 1.0-(hackAmount/TURRET_HACK_TIME), 1.0, 1.0);
  end;

  if (hackAmount > 0) then hackAmount -= compensation else if (hackAmount < 0) then hackAmount := 0;

  if (visible_) then
  begin
    shaderToUse^.bind;
    draw(false, true);
    tintShader^.bind;
  end;
  tintShader^.use;
  tintShader^.setUniform4(EXTRA1_LOCATION, 1.0, 1.0, 1.0, 1.0);

  hackedThisFrame := false;
  timeFromHackCompletion += compensation;
end;

function TTurret.getTarget : PGameActor;
begin
  result := target;
end;

procedure TTurret.setTarget(newTarget : PGameActor);
begin
  target := newTarget;
end;

procedure TTurret.hack;
var
  i : integer;
  tempSpambot : PSpambot;
begin
  if (not hackedThisFrame) then
  begin
    hackAmount += compensation*2;
    beingHacked := true;
    timeFromHack := 0;
    if (not hacked) then
    begin
      bufferText('!', x_, y_+30+sin(degToRad(hackAmount)*16)*2, z_, 1, 0.3, 1.0, 0.0, 0.0, 1.0, tenneryBold);
      hackedThisFrame := true;
    end;
    if (hackAmount >= TURRET_HACK_TIME) then
    begin
      hackAmount := 0;
      hacked := not hacked;
      beingHacked := false;
      timeFromHackCompletion := 0;

      if (hacked) then gameActors.add(new(PFloatText, create('!', x_, y_+40, z_, pictosWeb, 1.0, 1.0, 0.0, 0.4))) else
      begin
        gameActors.add(new(PFloatText, create('Hacked!', x_, y_+40, z_, tenneryBold, 1.0, 1.0, 0.0)));
        hackedSound^.play(round(100*soundEffectsVolume));
      end;

      if (spambots.count > 0) then
      begin
        for i := 0 to spambots.count-1 do
        begin
          tempSpambot := PSpambot(spambots[i]);
          if (tempSpambot^.getType = HACKER) then
          begin
            tempSpambot^.resetAttack;
            PHacker(tempSpambot)^.updateHackTarget;
          end;
        end;
      end;
    end;
  end;
end;

function TTurret.isHacked : boolean;
begin
  result := hacked;
end;

function TTurret.readyToUpgrade : boolean;
begin
  result := ((timeFromHackCompletion >= 60) and (not hacked));
end;

constructor TKittyTurret.create(destX, destZ, newRotation : real; alreadyHacked : boolean = false);
begin
  TGameObject.create(TURRET_CHARACTER+'kittyTurret', destX, 0.2, destZ, kittyTurretModel);
  initialRotation := newRotation;
  while (initialRotation >= 360) do initialRotation -= 360;
  while (initialRotation < 0) do initialRotation += 360;
  yRotation_ := initialRotation;
  hacked := alreadyHacked;
  init;
  rateOfFire := 5;
end;

procedure TKittyTurret.shoot;
var
  xDist, zDist, dist, volume : real;
begin
  gameActors.add(new(PBullet1, create(yRotation_, x_-(sin(degToRad(-yRotation_))*20), z_+(cos(degToRad(-yRotation_))*20))));
  if (cat <> nil) then
  begin
    xDist := x_-cat^.x;
    zDist := z_-cat^.z;
    dist := (xDist*xDist)+(zDist*zDist);
    if (dist > MAX_HEARING_DISTANCE_SQAURED) then exit;
    dist := sqrt(dist);

    volume := round(20*(100/dist));
    if (volume > 30) then volume := 30;
    kittyGunSound^.play(round(volume*soundEffectsVolume));
  end;
end;

constructor TAkatTurret.create(destX, destZ, newRotation : real; alreadyHacked : boolean = false);
begin
  TGameObject.create(TURRET_CHARACTER+'akatTurret', destX, 0.2, destZ, akatTurretModel);
  initialRotation := newRotation;
  while (initialRotation >= 360) do initialRotation -= 360;
  while (initialRotation < 0) do initialRotation += 360;
  yRotation_ := initialRotation;
  hacked := alreadyHacked;
  init;
  rateOfFire := 2;
end;

procedure TAkatTurret.update;
begin
  TTurret.update;
  if (lastShot < rateOfFire) then drawLine(x_-(sin(degToRad(-yRotation_))*20), 11, z_+(cos(degToRad(-yRotation_))*20), targetDistance, 2, yRotation_-90+((random(3)-1)*0.6), 0.2, 0.2, 0.2, 1.0);
end;

procedure TAkatTurret.shoot;
var
  xDist, zDist, dist, volume : real;
begin
  target^.doDamage(3);
  if (cat <> nil) then
  begin
    xDist := x_-cat^.x;
    zDist := z_-cat^.z;
    dist := (xDist*xDist)+(zDist*zDist);
    if (dist > MAX_HEARING_DISTANCE_SQAURED) then exit;
    dist := sqrt(dist);

    volume := round(20*(100/dist));
    if (volume > 30) then volume := 30;
    kittyGunSound^.play(round(volume*soundEffectsVolume));
  end;
end;

end.

