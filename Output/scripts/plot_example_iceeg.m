
close all; clearvars; clc % clear workspace

%% folders set up

cd /media/b6036780/8TB1/norm-ieeg-age-sex-site/Output/example-patients/MALE33/

% add plotting functions to path
addpath('~/MATLAB/lib/Viewing icEEG/')


%%  plot iEEG

% patient ID and site colour
patient_ID='RAMJ1277';
%patient_ID='RAMM1167';
%patient_ID='UCLH803';

col=[0.50 0.79 0.50]; % jefferson
%col=[0.75 0.68 0.83];  % mayo
%col=[0.99 0.75 0.53];  % uclh

% load workspace
load([patient_ID '.mat'])

% time vector, in seconds
t = (1:size(eeg_data,2))/eeg_fs;
disp(['Segment is ' num2str(max(t)) ' seconds long'])

% set plot options
plot_opts = struct();
plot_opts.offset = 200;          % higher number = more space between traces
plot_opts.plot_labels = false;    

% add channel labels to plot - currently not adding
plot_opts.labels = eeg_channels;

% colour (same for all chan)
colmat = repmat(col,height(eeg_channels),1);
plot_opts.clrs=colmat;

%%% plot EEG traces
        
% plot eeg
f=figure(3);
vis_plot_eeg(t,eeg_data,plot_opts);
set(gca,'XTickLabel',[])
set(f,'units','centimeters','position',[1 1 20 13]);
%title(patient_ID)
%ylabel('Channels')
%xlabel('Time')
     
% save
save_plot(f,pwd,patient_ID,'pdf')
%exportgraphics(f, [patient_ID '.pdf'], 'ContentType', 'vector')
