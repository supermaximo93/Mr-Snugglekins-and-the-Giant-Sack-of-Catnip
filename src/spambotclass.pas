unit SpambotClass;

{$mode objfpc}{$H+}

interface

uses GameActorClass, ExplosionClass;

const
  DRONE_SPEED = 0.5;
  HACKER_SPEED = 2;
  MECH_SPEED = 0.5;

  DRONE_DAMAGE = 1;
  HACKER_DAMAGE = 1;
  MECH_DAMAGE = 3;

type
  spambotTypeEnum = (DRONE, HACKER, MECH);

  PSpambot = ^TSpambot;
  TSpambot = object(TGameActor)
  protected
    moving, attacking : boolean;
    timeFromDamage, damage, rotationAmount : real;
    reward, standingFrame, avoidRotation : integer;
    obstacle, target : PGameActor;
    spambotType : spambotTypeEnum;
  public
    destructor destroy; virtual;
    procedure init;
    procedure update; virtual;
    procedure pausedDraw; virtual;

    function getType : spambotTypeEnum;

    function getObstacle : PGameActor;
    procedure setObstacle(newObstacle : PGameActor);

    function getTarget : PGameActor;
    procedure setTarget(newTarget : PGameActor);
    function rotationToTarget : real;
    function distanceToTargetSquared : real;

    procedure resetAttack;
  end;

  PDrone = ^TDrone;
  TDrone = object(TSpambot)
  public
    constructor create(destX, destZ : real; inMainMenu : boolean = false);
    procedure update; virtual;
  end;

  PHacker = ^THacker;
  THacker = object(TSpambot)
  public
    constructor create(destX, destZ : real);
    procedure update; virtual;
    procedure updateHackTarget;
  end;

  PMech = ^TMech;
  TMech = object(TSpambot)
  public
    constructor create(destX, destZ : real);
    procedure update; virtual;
  end;

procedure initSpambots;
procedure freeSpambots;

implementation

uses SysUtils, Classes, Math, Display, Input, GraphicalAssetClasses, ShaderClass,
     SoundClass, GlobalGameVariables, FloatScoreClass, TurretClass;

var
  droneModel, hackerModel, mechModel : PModel;
  sparkSprite : array[0..2] of PSprite;

procedure initSpambots;
var
  i : integer;
begin
  droneModel := new(PModel, create('drone', 'assets/models/drone/', 'drone.smo'));
  hackerModel := new(PModel, create('para', 'assets/models/hacker/', 'hacker.smo'));
  mechModel := new(PModel, create('mech', 'assets/models/mech/', 'mech.smo'));

  for i := 0 to 2 do sparkSprite[i] := new(PSprite, create('spark'+intToStr(i),
      'assets/sprites/spark/spark'+intToStr(i)+'.png', 0, 0, 50, 50, 1, 1, 25, 25));
end;

procedure freeSpambots;
var
  i : integer;
begin
  for i := 0 to 2 do dispose(sparkSprite[i], destroy);

  dispose(droneModel, destroy);
  dispose(hackerModel, destroy);
  dispose(mechModel, destroy);
end;

destructor TSpambot.destroy;
var
  i : integer;
  tempSpambot : PSpambot;
  tempTurret : PTurret;
begin
  collidables.delete(collidables.indexOf(@self));
  spambots.delete(spambots.indexOf(@self));

  if (not quit) then
  begin
    if (spambots.count > 0) then
    begin
      for i := 0 to spambots.count-1 do
      begin
        tempSpambot := PSpambot(spambots[i]);
        if (tempSpambot^.obstacle = @self) then tempSpambot^.obstacle := nil;
      end;
    end;
    if (turrets.count > 0) then
    begin
      for i := 0 to turrets.count-1 do
      begin
        tempTurret := PTurret(turrets[i]);
        if (tempTurret^.getTarget = @self) then tempTurret^.setTarget(nil);
      end;
    end;

    spambotKills[spambotType] += 1;
    randomDrop(x_, z_);
  end;
  TGameObject.destroy;
end;

procedure TSpambot.init;
begin
  dieFlag := false;
  moving := true;
  attacking := false;
  justDamaged := false;
  timeFromDamage := 0;
  obstacle := nil;
  avoidRotation := 1;
  collidables.add(@self);
  spambots.add(@self);
  target := sack;
  yRotation_ := rotationToTarget;
  attributes := [moveable, castsShadow];
  reward := SPAMBOT_REWARD[spambotType];
end;

procedure TSpambot.update;
var
  tempX, tempZ, xDist, zDist, angle, tempYRotation, tempAngle : real;
  i : integer;
  tempGameActor : PGameActor;
  collided : boolean = false;
  shaderToUse : PShader;
begin
  if (visible_) then
  begin
    if (health < initialHealth/3) then
    begin
      shaderToUse := tintAndWearShader;
      tintAndWearShader^.use;
      tintAndWearShader^.setUniform4(EXTRA1_LOCATION, 1.0, 1.0, 1.0, 1.0);
      tintAndWearShader^.setUniform1(EXTRA2_LOCATION, 1);
      tintAndWearShader^.setUniform1(EXTRA3_LOCATION, 1);
      bindDissolveMap;
    end else if (health < (initialHealth/3)*2) then
    begin
      shaderToUse := tintAndWearShader;
      tintAndWearShader^.use;
      tintAndWearShader^.setUniform4(EXTRA1_LOCATION, 1.0, 1.0, 1.0, 1.0);
      tintAndWearShader^.setUniform1(EXTRA2_LOCATION, 1);
      tintAndWearShader^.setUniform1(EXTRA3_LOCATION, 0);
      bindDissolveMap;
    end else shaderToUse := tintShader;
  end else shaderToUse := tintShader;

  shaderToUse^.bind;
  shaderToUse^.use;

  if (justDamaged) then
  begin
    if (timeFromDamage > DAMAGE_TINT_TIME) then
    begin
      justDamaged := false;
      timeFromDamage := 0;
    end else timeFromDamage += compensation;
    shaderToUse^.setUniform4(EXTRA1_LOCATION, 1.0, 0.2, 0.2, 1.0);
  end;

  if (moving) then
  begin
    tempX := x_;
    tempZ := z_;
    x_ += sin(degToRad(yRotation_))*speed*compensation;
    z_ += cos(degToRad(yRotation_))*speed*compensation;

    if (collidables.count > 0) then
    begin
      for i := 0 to collidables.count-1 do
      begin
        tempGameActor := PGameActor(collidables[i]);
        if (tempGameActor <> @self) then
        begin
          if (collision(tempGameActor)) then
          begin
            if (not (isHovering and tempGameActor^.isShort)) then
            begin
              collided := true;
              if (tempGameActor <> obstacle) then
              begin
                rotationAmount := 110*(abs(radToDeg(arcTan(speed/tempGameActor^.getRadius)))/360);
                if (tempGameActor^.name[1] = ENEMY_CHARACTER) then
                begin
                  if (PSpambot(tempGameActor)^.obstacle <> @self) then
                  begin
                    obstacle := tempGameActor;
                    xDist := x_-obstacle^.x;
                    zDist := z_-obstacle^.z;
                    if (zDist = 0) then zDist := 1;
                    angle := radToDeg(arcTan(xDist/zDist));
                    if (angle < 0) then avoidRotation := 1 else avoidRotation := -1;
                  end;
                end
              else
                begin
                  obstacle := tempGameActor;
                  xDist := x_-obstacle^.x;
                  zDist := z_-obstacle^.z;
                  if (zDist = 0) then zDist := 1;
                  angle := radToDeg(arcTan(xDist/zDist));
                  if (angle < 0) then avoidRotation := 1 else avoidRotation := -1;
                end;
              end;
              break;
            end;
          end;
        end;
      end;
    end;

    if (collided) then
    begin
      x_ := tempX;
      z_ := tempZ;
      if (target = PGameActor(sack)) then
      begin
        if ((distanceToTargetSquared <= MAXIMUM_SACK_DISTANCE_FOR_ATTACK_SQUARED) or
           ((spambotType = MECH) and (distanceToTargetSquared <= MAXIMUM_SACK_DISTANCE_FOR_ATTACK_SQUARED+50))) then
        begin
          moving := false;
          attacking := true;
          obstacle := nil;
          setCurrentAnimation(1);
        end;
      end
    else
      begin
        if (distanceToTargetSquared <= (target^.getRadius+10)*(target^.getRadius+10)) then
        begin
          moving := false;
          attacking := true;
          obstacle := nil;
          setCurrentAnimation(1);
        end;
      end;
      if (obstacle <> nil) then yRotation_ += avoidRotation*compensation;
    end
  else
    begin
      angle := rotationToTarget;
      if (yRotation_ <> angle) then
      begin
        yRotation_ -= avoidRotation*rotationAmount*compensation;
        tempYRotation := yRotation_;
        tempAngle := angle;
        if (angle+10 > 360) then
        begin
          tempAngle -= (angle+10)-360;
          tempYRotation -= (angle+10)-360;
        end else if (angle-10 < 0) then
        begin
          tempAngle -= angle-10;
          tempYRotation -= angle-10;
        end;
        if ((tempYRotation < tempAngle+10) and (tempYRotation > tempAngle-10)) then
        begin
          yRotation_ := angle;
          obstacle := nil;
        end;
      end else obstacle := nil;
    end;

    if (yRotation_ > 360) then yRotation_ -= 360 else if (yRotation_ < 0) then yRotation_ += 360;
    setFrame(1.5, true);
  end
else
  begin
    if (attacking) then
    begin
      setFrame(1.5, true);
      if (target = PGameActor(sack)) then sack^.doDamage(damage*compensation);
    end else setFrame(standingFrame);
  end;

  if (visible_) then
  begin
    draw(false, true);
    tintShader^.bind;
    tintShader^.use;
    tintShader^.setUniform4(EXTRA1_LOCATION, 1.0, 1.0, 1.0, 1.0);
    shadowCircle^.draw(x_, 0, z_, yRotation_, width_, width_);
  end;
end;

procedure TSpambot.pausedDraw;
var
  shaderToUse : PShader;
begin
  if (health < initialHealth/3) then
  begin
    shaderToUse := tintAndWearShader;
    tintAndWearShader^.use;
    tintAndWearShader^.setUniform4(EXTRA1_LOCATION, 1.0, 1.0, 1.0, 1.0);
    tintAndWearShader^.setUniform1(EXTRA2_LOCATION, 1);
    tintAndWearShader^.setUniform1(EXTRA3_LOCATION, 1);
    bindDissolveMap;
  end else if (health < (initialHealth/3)*2) then
  begin
    shaderToUse := tintAndWearShader;
    tintAndWearShader^.use;
    tintAndWearShader^.setUniform4(EXTRA1_LOCATION, 1.0, 1.0, 1.0, 1.0);
    tintAndWearShader^.setUniform1(EXTRA2_LOCATION, 1);
    tintAndWearShader^.setUniform1(EXTRA3_LOCATION, 0);
    bindDissolveMap;
  end
else
  begin
    shaderToUse := tintShader;
    shaderToUse^.use;
  end;

  if (justDamaged) then shaderToUse^.setUniform4(EXTRA1_LOCATION, 1.0, 0.2, 0.2, 1.0);

  shaderToUse^.bind;
  draw(false, true);
  tintShader^.bind;
  tintShader^.use;
  tintShader^.setUniform4(EXTRA1_LOCATION, 1.0, 1.0, 1.0, 1.0);
end;

function TSpambot.getType : spambotTypeEnum;
begin
  result := spambotType;
end;

function TSpambot.getObstacle : PGameActor;
begin
  result := obstacle;
end;

procedure TSpambot.setObstacle(newObstacle : PGameActor);
begin
  obstacle := newObstacle;
end;

function TSpambot.getTarget : PGameActor;
begin
  result := target;
end;

procedure TSpambot.setTarget(newTarget : PGameActor);
begin
  target := newTarget;
end;

function TSpambot.rotationToTarget : real;
var
  xDiff, zDiff, rot : real;
begin
  xDiff := x_-target^.x;
  if (xDiff = 0) then xDiff := 1;
  zDiff := z_-target^.z;
  rot := radToDeg(arcTan(zDiff/xDiff));
  if (rot < 0) then rot := -rot-90 else rot := -rot+90;
  if (zDiff > 0) then rot := rot+180;
  result := rot;
end;

function TSpambot.distanceToTargetSquared : real;
var
  xDist, zDist : real;
begin
  xDist := x_-target^.x;
  zDist := z_-target^.z;
  result := (xDist*xDist)+(zDist*zDist);
end;

procedure TSpambot.resetAttack;
begin
  attacking := false;
  moving := true;
  target := sack;
  obstacle := nil;
  setCurrentAnimation(0);
end;

constructor TDrone.create(destX, destZ : real; inMainMenu : boolean = false);
begin
  TGameObject.create(ENEMY_CHARACTER+'drone', destX, 0, destZ, droneModel);
  spambotType := DRONE;
  if (not inMainMenu) then init;
  speed := DRONE_SPEED;
  damage := DRONE_DAMAGE;
  initialHealth := 30;
  health := initialHealth;
  radius := 7.5;
  standingFrame := 15;
  width_ := 0.3;
end;

procedure TDrone.update;
begin
  TSpambot.update;

  if (health < 1) then
  begin
    dieFlag := true;
    gameActors.add(new(PExplosion, create(x_, y_+12, z_, 0.5)));
    gameActors.add(new(PFloatScore, create(reward, x_, y_+20, z_)));
  end;
end;

constructor THacker.create(destX, destZ : real);
begin
  TGameObject.create(ENEMY_CHARACTER+'hacker', destX, 10, destZ, hackerModel);
  spambotType := HACKER;
  init;
  speed := HACKER_SPEED;
  damage := HACKER_DAMAGE;
  initialHealth := 30;
  health := initialHealth;
  radius := 7.5;
  reward := 10;
  standingFrame := 0;
  attributes += [hovering];
  width_ := 0.3;
  updateHackTarget;
end;

procedure THacker.update;
begin
  TSpambot.update;

  if (attacking) then
    bufferSprite(sparkSprite[random(3)], x_+sin(degToRad(yRotation_))*10+sin(degToRad(yRotation_-90))*4,
      y_-1.5, z_+cos(degToRad(yRotation_))*10+cos(degToRad(yRotation_-90))*4, 0.2, 1.0, 1.0, 1.0, 1.0);

  if (target <> nil) then
  begin
    if (attacking and (target^.name[1] = TURRET_CHARACTER)) then PTurret(target)^.hack;
  end;

  if (health < 1) then
  begin
    dieFlag := true;
    gameActors.add(new(PExplosion, create(x_, y_+12, z_, 0.5)));
    gameActors.add(new(PFloatScore, create(reward, x_, y_+20, z_)));
  end;
end;

procedure THacker.updateHackTarget;
var
  i : integer;
  distanceSquared, tempDist : longword;
  tempTurret : PGameActor = nil;
begin
  distanceSquared := 4294967290;
  if (target = PGameActor(sack)) then
  begin
    if (turrets.count > 0) then
    begin
      for i := 0 to turrets.count-1 do
      begin
        target := PGameActor(turrets[i]);
        if (PTurret(target)^.isHacked) then continue;
        tempDist := round(distanceToTargetSquared);
        if (tempDist < distanceSquared) then
        begin
          distanceSquared := tempDist;
          tempTurret := target;
        end;
      end;
    end;
  end;
  if (tempTurret <> nil) then target := tempTurret else target := sack;
end;

constructor TMech.create(destX, destZ : real);
begin
  TGameObject.create(ENEMY_CHARACTER+'mech', destX, -1.5, destZ, mechModel);
  spambotType := MECH;
  init;
  speed := MECH_SPEED;
  damage := MECH_DAMAGE;
  initialHealth := 150;
  health := initialHealth;
  radius := 20;
  reward := 30;
  standingFrame := 30;
  width_ := 0.8;
end;

procedure TMech.update;
begin
  TSpambot.update;

  if (health < 1) then
  begin
    dieFlag := true;
    gameActors.add(new(PExplosion, create(x_, y_+30, z_, 1.0)));
    gameActors.add(new(PFloatScore, create(reward, x_, y_+50, z_)));
  end;
end;

end.

