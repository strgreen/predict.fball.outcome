clear all

% [num txt raw] = xlsread('C:\Users\Baruch Spinoza\Google Drive\GreenAnalytics\predict.fball.outcome\neb.home.games.xlsx');
[num txt raw] = xlsread('C:\Users\Baruch Spinoza\Google Drive\GreenAnalytics\predict.fball.outcome\all.games.xlsx');

%create time column (stored in seconds)

%Initialize some variables
gameCount = 0;
playCount = 0;

vstScore = 0;
homScore = 0;

count = 0;
for index = 2:size(raw,1)-1
    
    if isnan(raw{index,5}) == 1
    else
        
        
        %Chcek to see if we are in a new game.
        if strcmp(raw{index,2}, raw{index-1,2})== 0 | ...
                strcmp(raw{index,3}, raw{index-1,3}) == 0
            
            gameCount = gameCount + 1;
            
        else
            
            playCount = playCount + 1;
            
            count = count + 1;
            
            gameArray{count,1} = gameCount;
            gameArray{count,2} = raw{index,2};              %Home Team
            gameArray{count,3} = raw{index,3};              %Visitor Team
            
            
            gameArray{count,4} = raw{index,4}(1);           %Quarter
            
            %Convert time into seconds
            %Place a zero for entires with no minute place.
            if strcmp(raw{index,5}(1), ':') == 1
                raw{index,5} = ['0' raw{index,5} ];
            end
            
            temp  = datestr(raw{index,5},'HH:MM:SS');
            gameArray{count,5} = str2num(temp(1:2))*60 + str2num(temp(4:5));    %Time(seconds) drive starts
            
            gameArray{count,6} = raw{index,28};         %Who Has the ball
            
            if strcmp(raw(index,2), raw(index,28)) == 1
                gameArray{count,7} = 1;                 %Home team has ball
            else
                gameArray{count,7} = 0;                 %Home Team does not have the ball
            end
            
            
            if isnan(raw{index,6}) == 0
                gameArray{count,8} = raw{index,6};      %Down
            else
                gameArray{count,8} = 0;
            end
            
            gameArray{count,9} = raw{index,10};         %Play Type
            gameArray{count,10} = raw{index,13};         %Pass Completed?
            
            if isnan(raw{index,16}) == 0
                gameArray{count,11} = raw{index,16};     %Yards
            else
                gameArray{count,11} = 0;
            end
            
            gameArray{count,12} =  raw{index,17};       %vst score
            gameArray{count,13} = raw{index,18};        %hom score
            
            %Check for interception
            if isempty(findstr(raw{index,19},'INTERCEPTED')) == 0
                gameArray{count,14} = 1; %Intercepted
            else
                gameArray{count,14} = 0;
            end
            
            %Check for fumble
            if isempty(findstr(raw{index,19},'FUMBLES')) == 0
                gameArray{count,15} = 1; %Fumbles
            else
                gameArray{count,15} = 0;
            end
            
            %check for first down
            if raw{index,16} > raw{index,7} & strcmp(raw{index,10}, 'PUNT') ==0 ...
                    & strcmp(raw{index,10}, 'KICK') ==0
                gameArray{count,16} = 1;                %1st down
            else
                gameArray{count,16} = 0;
            end
            
        end
        
    end
end

%Build a game summary

gameArray = sortrows(gameArray,[1, 4,-5]);

for game = 1:gameArray{end,1}
    
    gameData = gameArray(find(cell2mat(gameArray(:,1))==game),:);
    
    fin_hom_score = max(cell2mat(gameData(:,13)));
    fin_vst_score = max(cell2mat(gameData(:,12)));

    %Game outcome
    if fin_hom_score > fin_vst_score
        gameOutcome = 1;
    else
        gameOutcome = 0; 
    end
    
    fin_hom_yards = sum(cell2mat(gameData(cell2mat(gameData(:,7)) == 1,11)));
    fin_vst_yards = sum(cell2mat(gameData(cell2mat(gameData(:,7)) == 0,11)));
    yrdRatio = fin_hom_yards/fin_vst_yards;
    
    fin_hom_fdown = sum(cell2mat(gameData(cell2mat(gameData(:,7)) == 1,16)));
    fin_vst_fdown = sum(cell2mat(gameData(cell2mat(gameData(:,7)) == 0,16)));
    fdownRatio = fin_hom_fdown/fin_vst_fdown;
    
    fin_hom_inter = sum(cell2mat(gameData(cell2mat(gameData(:,7)) == 1,14)));
    fin_vst_inter = sum(cell2mat(gameData(cell2mat(gameData(:,7)) == 0,14)));
    interContr = fin_hom_inter - fin_vst_inter;
    
    fin_hom_fmble = sum(cell2mat(gameData(cell2mat(gameData(:,7)) == 1,15)));
    fin_vst_fmble = sum(cell2mat(gameData(cell2mat(gameData(:,7)) == 0,15)));
    fmbleContr = fin_hom_fmble - fin_vst_fmble;
        
    fin_hom_trnOvr = fin_hom_inter + fin_hom_inter;
    fin_vst_trnOvr = fin_vst_inter + fin_vst_inter;
    trnOvrContr = fin_hom_trnOvr - fin_vst_trnOvr; 
    
    outArray{game,1} = game;
    outArray{game,2} = gameOutcome; 
    
    outArray{game,3} = gameData{1,2};
    outArray{game,4} = gameData{1,3};
    
    outArray{game,5} = fin_hom_score;
    outArray{game,6} = fin_vst_score;
    outArray{game,7} = fin_hom_yards;
    outArray{game,8} = fin_vst_yards;
    outArray{game,9} = fin_hom_fdown;
    outArray{game,10} = fin_vst_fdown;
    
    outArray{game,11} = fin_hom_inter;
    outArray{game,12} = fin_vst_inter;
    outArray{game,13} = fin_hom_fmble;
    outArray{game,14} = fin_vst_fmble;
    outArray{game,15} = fin_hom_trnOvr;
    outArray{game,16} = fin_vst_trnOvr;

    
    outArray{game,17} = yrdRatio;
    outArray{game,18} = fdownRatio;
    outArray{game,19} = interContr;
    outArray{game,20} = fmbleContr;
    outArray{game,21} = trnOvrContr;
    
end

header = {'Game', 'Outcome', 'Home Team', 'Visitor Team', 'Home Score', 'Visitor Score', ...
    'Home Yards', 'Visitor Yards', 'Home 1st Downs','Vistior 1st Downs', ...	
    'Home Interceptions', 'Visitor Interceptions', 'Home Fumble', ...	
    'Visitor Fumble', 'Home Turnovers', 'Visitor Turnovers', 'Yard Ratio', ...
    'First Down Ratio', 'Interception Contrast'	...
    'Fumble Contrast' 'Turnover Contrast' };

