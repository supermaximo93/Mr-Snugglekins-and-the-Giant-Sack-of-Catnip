unit FloatScoreClass;

{$mode objfpc}{$H+}

interface

uses GraphicalAssetClasses, FontClass, GameActorClass, GlobalGameVariables;

type
  PFloatScore = ^TFloatScore;
  TFloatScore = object(TGameActor)
  private
    time : real;
    text : string;
  public
    constructor create(newScore : integer; destX, destY, destZ : real);
    destructor destroy; virtual;
    procedure update; virtual;
    procedure pausedDraw; virtual;
  end;

  PFloatText = ^TFloatText;
  TFloatText = object(TGameActor)
  private
    time, r, g, b : real;
    text : string;
    font : PFont;
  public
    constructor create(newText : string; destX, destY, destZ : real; fontToUse : PFont; newR : real = 1.0; newG : real = 1.0; newB : real = 1.0; newScale : real = 0.1);
    destructor destroy; virtual;
    procedure update; virtual;
    procedure pausedDraw; virtual;
  end;

  PFloatSprite = ^TFloatSprite;
  TFloatSprite = object(TGameActor)
  private
    time, floatSpeed, fadeSpeed : real;
    spriteToUse : PSprite; //Not using the sprite_ field inherited from TGameObject so that lines aren't drawn over the sprite in the line draw cycle
  public
    constructor create(newSprite : PSprite; destX, destY, destZ : real; newScale : real = 1.0; newFloatSpeed : real = 1; newFadeSpeed : real = 1);
    destructor destroy; virtual;
    procedure update; virtual;
    procedure pausedDraw; virtual;
  end;

implementation

uses SysUtils, Display;

constructor TFloatScore.create(newScore : integer; destX, destY, destZ : real);
begin
  TGameObject.create('floatScore', destX, destY, destZ, PModel(nil));
  time := 0;
  dieFlag := false;
  text := '$'+intToStr(newScore);
  width_ := tenneryBold^.width('$'+intToStr(score))*0.1;
  attributes := [];
  updateVisibility;
  if (not visible_) then dieFlag := true;
  if (euphoriaBonus = 1) then euphoriaBuildup += newScore*0.1;
  money += newScore;
  score += newScore;
end;

destructor TFloatScore.destroy;
begin
  TGameObject.destroy;
end;

procedure TFloatScore.update;
begin
  if ((time > 100) or (not visible_)) then dieFlag := true;
  bufferText(text, x_, y_, z_, width_/2, 0.1, 0.0, 1.0, 0.0,  1-(time*0.02), tenneryBold);
  time += compensation;
  y_ += compensation*0.1;
end;

procedure TFloatScore.pausedDraw;
begin
  bufferText(text, x_, y_, z_, width_/2, 0.1, 0.0, 1.0, 0.0,  1-(time*0.02), tenneryBold);
end;

constructor TFloatText.create(newText : string; destX, destY, destZ : real; fontToUse : PFont; newR : real = 1.0; newG : real = 1.0; newB : real = 1.0; newScale : real = 0.1);
begin
  TGameObject.create('floatText', destX, destY, destZ, PModel(nil));
  time := 0;
  dieFlag := false;
  text := newText;
  font := fontToUse;
  r := newR;
  g := newG;
  b := newB;
  xScale_ := newScale;
  width_ := (fontToUse^.width(newText)*newScale)/2;
  attributes := [];
  updateVisibility;
  if (not visible_) then dieFlag := true;
end;

destructor TFloatText.destroy;
begin
  TGameObject.destroy;
end;

procedure TFloatText.update;
begin
  if ((time > 100) or (not visible_)) then dieFlag := true;
  bufferText(text, x_, y_, z_, width_, xScale_, r, g, b,  1-(time*0.02), font);
  time += compensation;
  y_ += compensation*0.1;
end;

procedure TFloatText.pausedDraw;
begin
  bufferText(text, x_, y_, z_, width_, xScale_, r, g, b,  1-(time*0.02), font);
end;

constructor TFloatSprite.create(newSprite : PSprite; destX, destY, destZ : real; newScale : real = 1.0; newFloatSpeed : real = 1; newFadeSpeed : real = 1);
begin
  TGameObject.create('floatSprite', destX, destY, destZ, PModel(nil));
  spriteToUse := newSprite;
  time := 0;
  dieFlag := false;
  scale(newScale, newScale, 1);
  floatSpeed := newFloatSpeed;
  fadeSpeed := newFadeSpeed;
  updateVisibility;
  attributes := [];
end;

destructor TFloatSprite.destroy;
begin
  TGameObject.destroy;
end;

procedure TFloatSprite.update;
begin
  if ((time > 300) or (not visible_)) then dieFlag := true;
  bufferSprite(spriteToUse, x_, y_, z_, xScale, 1.0, 1.0, 1.0, 1-(time*0.004));
  time += compensation*fadeSpeed;
  y_ += compensation*0.1*floatSpeed;
end;

procedure TFloatSprite.pausedDraw;
begin
  bufferSprite(spriteToUse, x_, y_, z_, xScale, 1.0, 1.0, 1.0, 1-(time*0.004));
end;

end.

