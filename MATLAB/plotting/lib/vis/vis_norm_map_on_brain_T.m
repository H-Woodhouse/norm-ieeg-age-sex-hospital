function [f, ax] = vis_norm_map_on_brain_T(norm_meas,feat_names,atlas,options)
% VIS_NORM_MAP_ON_BRAIN Plot normative map (all features) on brain surface
% in one or more views.
%
% Creates a new figure with views x features subplots.
%
% Calls vis_plot_abnormalities_on_brain with options that work well for
% this application. 
%
%   [f, ax] = VIS_NORM_MAP_ON_BRAIN(norm_meas,feat_names,atlas) returns the 
%   figure and axes handles, f and x, to the plots of the normative map, 
%   norm_meas (ROIs x features matrix). Each column of subplots corresponds
%   to a different feature whose names are given in the cell array 
%   feat_names. ROI info is passed via atlas, a table that must contain 
%   variables "names" (cell arrays of ROI names) and "xyz" (ROI xyz 
%   coordinates). Atlases are saved  in this format in the 
%   ieeg-norm-map-pipeline repository.By default, the first row of atlas 
%   table is used  for the ROI info, but a different row can be specified 
%   using the optional argument "AtlasIndex".
%
%   See argument validation comments for Name/Value pairs that provide 
%   limited customisation (e.g., views to plot). 
%
% See also VIS_PLOT_ABNORMALITES_ON_BRAIN.
%
% Gabrielle M. Schroeder
% CNNP Lab, Newcastle University
% February 2023

arguments
    norm_meas {mustBeNumeric}
    feat_names cell
    atlas table
    options.View cell = {'top','anterior','left','right'};  % must be subset of the default views
    options.MarkerSize (1,1) = 100                          % size to plot ROI markers
    options.Colormap = 'parula';    % colormap color
    options.ColorbarLocation = 'southoutside';              % colorbar location; see colorbar options
    options.CLim (:,2) = [min(norm_meas)' max(norm_meas)']; % Colorbar axis limits - can either be 1x2 vector (same limits for all features) or n feature x 2 array (different limits for each features). Defaults to min and max of each feature.
    options.PlotBrain = true        % whether to plot brain surface (can be useful to just export markers in epsc format - surface needs to be exported as non-vector graphic)
    options.FontSize = 16           % fontsize (colorbar labels only)
    options.TitleFontSize = 16;     % size of title (feature names)
    options.AtlasIndex = 1;
    options.MarkerEdgeOff = true ;  % whether to turn off plotting marker edges in a different colour; LineWidthSpared will still influence total marker size
    options.LineWidth = 0.5;        % marker linewidth
    options.MarkerEdgeColor = [0 0 0]; % marker color if MarkerEdgeOff = false
end

% number of features in map
n_feat = size(norm_meas,2);

% number of views to plot brain
n_view = length(options.View);

% expand colorbar limits if not provided for each feature
if size(options.CLim,1) == 1
    options.CLim = repmat(options.CLim,n_feat,1);
elseif size(options.CLim,1) ~= n_feat
    error('Colorbar limits CLim should either be size 1 x 2 or size n features x 2')
end

% start figure and set size
f=figure();
set(f,'units','centimeters','position',[2 2 10+n_feat*7 10+n_view*6])

% counter for subplots
my_count=1; 

% plot for each view and feature
for j=1:n_feat
    for i=1:n_view
        
    % which rois to plot (0 = plot, NaN = missing from map)
    x = single(isnan(norm_meas(:,j)));
    x(x==1) = NaN;
        
    % only plot colorbar for last view 
    if j == n_feat && i == n_view
        cbar = true;
    else
        cbar = false;
    end
    
    ax(i,j) = subplot(n_feat,n_view,my_count);
    pos = ax(i,j).Position;
    
    % call vis_plot_abnormalities_on_brain
    vis_plot_abnormalities_on_brain(norm_meas(:,j),x,...
        atlas.names{options.AtlasIndex},atlas.xyz{options.AtlasIndex},...
        'SparedMarkerEdgeOff',true,'SparedColor',options.MarkerEdgeColor',...
        'PlotNewFig',false,'MarkerSize',options.MarkerSize,'View',options.View{i},...
        'PlotColorbar',cbar,'Colormap',options.Colormap,'ColorbarLocation',...
        options.ColorbarLocation,'CLim',options.CLim(j,:),...
        'FontSize',options.FontSize,'PlotBrain',options.PlotBrain,...
        'SparedMarkerEdgeOff',options.MarkerEdgeOff,'LineWidthSpared',options.LineWidth);
    
    % return subplot to full size if has colorbar
    if i == n_view
        ax(i,j).Position = pos;
    end
    
    % title
    if i == 1
        title(feat_names{j},'FontSize',options.TitleFontSize)
    end
    
    % subplot counter
    my_count = my_count+1;
    
    end
end