unit HudClass;

{$mode objfpc}{$H+}

interface

uses GraphicalAssetClasses;

type
  //The heads up display that the player sees when the game is in action
  PHud = ^THud;
  THud = object
  private
    healthSprite, ammoSprite, sackSprite, headSprite, lineSprite, scoreSprite : PSprite;
    timeSinceCatJustDamaged : real;
  public
    constructor create;
    destructor destroy; virtual;
    procedure update; virtual;
  end;

implementation

uses SysUtils, Display, ShaderClass, FontClass, GlobalGameVariables;

constructor THud.create;
begin
  healthSprite := new(PSprite, create('hudHealth', 'assets/sprites/hud_cat_health.png', 0, 0, 129, 133));
  ammoSprite := new(PSprite, create('hudAmmo', 'assets/sprites/hud_ammo.png', 0, 0, 247, 133));
  sackSprite := new(PSprite, create('hudSack', 'assets/sprites/hud_sack_health.png', 0, 0, 400, 57));
  headSprite := new(PSprite, create('hudHead', 'assets/sprites/hud_cat_head.png', 0, 0, 105, 105));
  lineSprite := new(PSprite, create('hudLine', 'assets/sprites/line.png', 0, 0, 1, 40));
  scoreSprite := new(PSprite, create('hudScore', 'assets/sprites/hud_score.png', 0, 0, 238, 57));
  timeSinceCatJustDamaged := 10;
end;

destructor THud.destroy;
begin
  dispose(scoreSprite, destroy);
  dispose(lineSprite, destroy);
  dispose(headSprite, destroy);
  dispose(sackSprite, destroy);
  dispose(ammoSprite, destroy);
  dispose(healthSprite, destroy);
end;

procedure THud.update;
var
  cutoff : real;
  tempStr, clipSizeStr : string;
  i : integer;
begin
  //If the cat has just been damaged then make the screen flash red
  if (cat = nil) then
  begin
    timeSinceCatJustDamaged := 10;
    exit;
  end;
  if (catJustDamaged) then timeSinceCatJustDamaged := 0;

  fontShader^.use;
  fontShader^.setUniform4(EXTRA0_LOCATION, 1.0, 0.0, 0.0, 0.5*(1-(timeSinceCatJustDamaged/10)));
  whiteScreen^.draw(0, 0, -1, 0, skyXScale, skyYScale);
  if (timeSinceCatJustDamaged < 10) then timeSinceCatJustDamaged += compensation else timeSinceCatJustDamaged := 10;

  //Draw the HUD
  healthSprite^.draw(0, screenHeight-133, -1);
  ammoSprite^.draw(screenWidth-247, screenHeight-133, -1);
  sackSprite^.draw(0, 0, -1);

  fontShader^.use;
  fontShader^.setUniform4(EXTRA0_LOCATION, 0.0, 0.0, 0.0, 1.0);

  lineSprite^.bindShader(fontShader);
  lineSprite^.draw(7, 6, -1, 0, 380);
  if (sack^.getHealth > 0) then
  begin
    lineSprite^.bindShader(spriteShader);
    lineSprite^.draw(7, 6, -1, 0, (sack^.getHealth/sack^.getInitialHealth)*380);
  end;

  headSprite^.bindShader(fontShader);
  headSprite^.draw(10, screenHeight-118, -1);
  pictosWeb^.write('e', screenWidth-60, screenHeight-125, -1, true, 0, 1.5, 1.5);

  cutoff := (1-(cat^.getHealth/cat^.getInitialHealth))*105;
  drainVerticalShader^.use;
  drainVerticalShader^.setUniform4(EXTRA0_LOCATION, 1.0, 1.0, 1.0, 1.0);
  drainVerticalShader^.setUniform1(EXTRA1_LOCATION, cutoff);
  headSprite^.bindShader(drainVerticalShader);
  headSprite^.draw(10, screenHeight-118, -1);

  bindFontShader(drainVerticalShader);
  cutoff := (1-(euphoriaBuildup/100))*pictosWeb^.height('e');
  drainVerticalShader^.use;
  drainVerticalShader^.setUniform1(EXTRA1_LOCATION, cutoff);
  pictosWeb^.write('e', screenWidth-60, screenHeight-125, -1, true, 0, 1.5, 1.5);
  bindFontShader(fontShader);

  if (money >= A_LOT_OF_MONEY) then tempStr := 'A lot!' else tempStr := '$'+intToStr(money);
  tenneryBold^.write(tempStr, screenWidth-5-round(tenneryBold^.width(tempStr)*0.6),
                              screenHeight-40, -1, false, 0, 0.6, 0.6);

  fontShader^.use;
  if (not cat^.gun^.isReloaded) then fontShader^.setUniform4(EXTRA0_LOCATION, 1.0, 0.3, 0.3, 1.0);
  clipSizeStr := '';
  if (cat^.gun^.getClipSize > 0) then for i := 1 to cat^.gun^.getClipSize do clipSizeStr += '|';
  if (clipSizeStr = '') then clipSizeStr := ' ';
  tenneryBold^.write(clipSizeStr, screenWidth-235, screenHeight-53, -1, true, 0,
                                  1/(tenneryBold^.width(clipSizeStr)/133));

  fontShader^.use;
  fontShader^.setUniform4(EXTRA0_LOCATION, 1.0, 1.0, 1.0, 1.0);
  tempStr := '';
  if (cat^.gun^.getClip > 0) then for i := 1 to cat^.gun^.getClip do tempStr += '|';
  if (tempStr = '') then tempStr := ' ';
  tenneryBold^.write(tempStr, screenWidth-235, screenHeight-53, -1, true, 0,
                              1/(tenneryBold^.width(clipSizeStr)/133));

  scoreSprite^.draw(screenWidth-238, -15, -1);
  tempStr := 'Score: '+intToStr(score);
  tenneryBold^.write(tempStr, screenWidth-227, 8, -1, false, 0, 0.5, 0.5);
end;

end.

