% %
% % Script to visualise regression coefficients at the ROI level
% %
% % FIGURE 5B:
% % plot regional beta_age values on the brain (mirrored data is used but
% % results are reflected across the midline)
% %
% % REQUIRE the data frame created and saved in 'regional_age_model_stats.R'
% %
% %
%% directories and settings

% set working directory to script location (2_analysis)
cd(fileparts(matlab.desktop.editor.getActive().Filename))

% pipeline containing brain plotting functions
path_plot_fn = '../MATLAB_lib/plotting/';
addpath(genpath(path_plot_fn))

% Fieldtrip for brain surface plots
path_fieldtrip = '../MATLAB_lib/fieldtrip-20230215';  
addpath(genpath(path_fieldtrip));
rmpath(genpath([path_fieldtrip '/compat'])); % remove fieldtrip compatibility folder so don't duplicate matlab functions

clear path_fieldtrip path_plot_fn

%% load and format data

bands = {'delta','theta','alpha','beta','gamma'};

% load data
regional_summaries = readtable('../3_output/regional_age_model_stats_TEST.csv','VariableNamingRule','preserve');

% rename column to be what plotting function is looking for
regional_summaries = renamevars(regional_summaries,"ROI_name","names");

% retain just what's needed for plotting
% (x,y,z,names,beta_age coefficients in each region and band)
beta_ages = regional_summaries(:,["names","x","y","z",strcat(bands,"_coef")]);

% convert coefficients to matrix for plotting function
beta_ages_matrix=table2array(beta_ages(:,strcat(bands, '_coef')));

% create coordinate/atlas table for plotting function
% in ROI1, 76 regions because we removed 3 ROI on each hemisphere
atlas_cortical=table;
atlas_cortical.xyz=mat2cell([beta_ages.x,beta_ages.y,beta_ages.z],76,3);
atlas_cortical.names = mat2cell(beta_ages.names,76);       

clear regional_summaries beta_ages

%% plot maps and save (all FB)

%%%%% ISSUES ->
% plotting doesn't plot the correct colours the first time, run
% once, delete empty fig 1, LEAVE incorrect fig 2 open then run again
% resulting figure 1 is correct

% title labels
title_labels = {'δ','θ','α','β','γ'};

% find limits for CLim (need symmetric)
max(beta_ages_matrix)
min(beta_ages_matrix)

% plot
% can show/hide band titles in function code
vis_norm_map_on_brain_T(beta_ages_matrix,bands,title_labels,atlas_cortical,'Colormap',bluewhitered(256), ...
    'CLim',[-0.0015,0.0015],'View',{'top'}, 'FontSize',20, ...
    'ColorbarLocation','westoutside','TitleFontSize',42)

% save (too complex for vector format)
saveas(gcf, '../3_output/regional_beta_age_brains.png')
