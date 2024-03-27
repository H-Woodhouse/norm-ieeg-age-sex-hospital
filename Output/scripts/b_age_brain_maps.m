% % Script to produce the brain plots, which show regression 
% % coefficients at the ROI level

% % the function requires data in a specified format, this is 
% % created and saved in 'age_model_plots.R'

% % run for main results and also supplementary 


%% directories and settings

% working directory
% check (main results or supp?)
cd("/media/b6036780/8TB1/norm-ieeg-age-sex-site/Output/")

% pipeline containing brain plotting functions
path_plot_fn = '/media/b6036780/8TB1/norm-ieeg-age-sex-site/MATLAB/plotting/';
addpath(genpath(path_plot_fn))

% Fieldtrip for brain surface plots
path_fieldtrip = '/media/b6036780/8TB1/norm-ieeg-age-sex-site/MATLAB/fieldtrip-20230215';  
addpath(genpath(path_fieldtrip));
rmpath(genpath([path_fieldtrip '/compat'])); % remove fieldtrip compatibility folder so don't duplicate matlab functions

%% load data

% model summaries in required format from R
bands = {'delta','theta','alpha','beta','gamma'};
b_ages = readtable("b_age_coeffs_ROI1.csv");               % check file
b_ages_matrix=table2array(b_ages(:,strcat(bands, '_coef')));
title_labels = {'δ','θ','α','β','γ'};

% create atlas table (correct format for function)
% in RIO1, 76 regions bc removed 3 ROI on each hemisphere
atlas_cortical=table;
atlas_cortical.xyz=mat2cell([b_ages.x,b_ages.y,b_ages.z],76,3);
atlas_cortical.names = mat2cell(b_ages.names,76);             % check number of ROIs

%% plot maps and save (all FB)

%%%%% ISSUES ->
% plotting doesn't plot the correct colours the first time, run
% once, delete empty fig 1, LEAVE incorrect fig 2 open then run again
% resulting figure 1 is correct

% find limits for CLim (need symmetric)
max(b_ages_matrix)
min(b_ages_matrix)

% plot
% can show/hide band titles in function code
vis_norm_map_on_brain_T(b_ages_matrix,bands,title_labels,atlas_cortical,'Colormap',bluewhitered(256), ...
    'CLim',[-0.0015,0.0015],'View',{'top','left'}, 'FontSize',20, ...
    'ColorbarLocation','westoutside','TitleFontSize',42)

% save (too complex for vector format)
saveas(gcf, 'age_coeffs_map_ROI1_new_clim.png')
