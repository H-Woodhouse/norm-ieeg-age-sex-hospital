% % Script to produce the brain plots, which show regression 
% % coefficients at the ROI level

% % the function requires data in a specified format, this is 
% % created and saved in 'age model plots.R'

% % run for main results and also supplementary 


%% directories and settings

% working directory
% check (main or supp?)
cd("/media/b6036780/8TB1/norm-ieeg-age-sex-site/Output/sup-results/")

% pipeline containing brain plotting functions
path_plot_fn = '/media/b6036780/8TB1/norm-ieeg-age-sex-site/MATLAB/plotting';
addpath(genpath(path_plot_fn))

% Fieldtrip for brain surface plots
path_fieldtrip = '/media/b6036780/8TB1/norm-ieeg-age-sex-site/MATLAB/fieldtrip-20230215';  
addpath(genpath(path_fieldtrip));
rmpath(genpath([path_fieldtrip '/compat'])); % remove fieldtrip compatibility folder so don't duplicate matlab functions

%% load data

% model summaries in required format from R
bands = {'delta','theta','alpha','beta','gamma'};
betas = readtable("beta_age_coeffs_ROI2_SUP.csv");               % check file
betas_matrix=table2array(betas(:,strcat(bands, '_coef')));

% create atlas table (correct format for function)
% in RIO1, 76 regions bc removed 3 ROI on each hemisphere
atlas_cortical=table;
atlas_cortical.xyz=mat2cell([betas.x,betas.y,betas.z],122,3);
atlas_cortical.names = mat2cell(betas.names,122);             % check number of ROIs

%% plot maps and save (all FB)

%%%%% ISSUES ->
% plotting doesn't plot the correct colours the first time, run
% once, delete empty fig 1, LEAVE incorrect fig 2 open then run again
% resulting figure 1 is correct

% find limits for CLim (need symmetric)
max(betas_matrix)
min(betas_matrix)

% plot
% can show/hide band titles in function 
vis_norm_map_on_brain_T(betas_matrix,bands,atlas_cortical,'Colormap',bluewhitered(256), ...
    'CLim',[-0.0025,0.0025],'View',{'top','anterior','left','right'}, 'FontSize',24,'ColorbarLocation','southoutside')

% save (too complex for vector format)
saveas(gcf, 'age_coeffs_map_ROI2_SUP.png')
