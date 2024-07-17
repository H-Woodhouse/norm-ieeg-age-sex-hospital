
close all; clearvars; clc % clear workspace

%% folders set up

cd /media/b6036780/8TB1/norm-ieeg-age-sex-site/Output/example-patients/MALE33/

% add plotting functions to path
addpath('~/MATLAB/lib/Viewing icEEG/')


%%  plot iEEG

patient_ID='UCLH803';
%col=[0.50 0.79 0.50]; % jefferson
col=[0.99 0.75 0.53];  % uclh
%col=[0.75 0.68 0.83];  % mayo

% set plot options
plot_opts = struct();
plot_opts.offset = 200;     % higher number = more space between traces
plot_opts.plot_labels = true;    % plot labels (supplied in loop)
fig_num=1;


% load workspace
load([patient_ID '.mat'])

% time vector, in seconds
t = (1:size(eeg_data,2))/eeg_fs;
disp(['Segment is ' num2str(max(t)) ' seconds long']) % print segment length

% add channel labels to plot
plot_opts.labels = eeg_channels;

% colour (same for all chan)
colmat = repmat(col,height(eeg_channels),1);
plot_opts.clrs=colmat;

%%% plot EEG traces
        
% plot eeg
f=figure(1);
vis_plot_eeg(t,eeg_data,plot_opts);
set(f,'units','centimeters','position',[2 2 50 30],'visible','off'); % set figure position and size
title(patient_ID)
     
% save
save_plot(f,pwd,patient_ID,'jpeg')
