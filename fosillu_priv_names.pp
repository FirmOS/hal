unit fosillu_priv_names;

interface

 const
   PRIV_CONTRACT_IDENTITY : pchar = 'contract_identity'+#0;
   PRIV_CONTRACT_OBSERVER : pchar = 'contract_observer'+#0;
   PRIV_CPC_CPU : pchar = 'cpc_cpu'+#0;
   PRIV_DTRACE_KERNEL : pchar = 'dtrace_kernel'+#0;
   PRIV_DTRACE_PROC : pchar = 'dtrace_proc'+#0;
   PRIV_DTRACE_USER : pchar = 'dtrace_user'+#0;
   PRIV_FILE_CHOWN : pchar = 'file_chown'+#0;
   PRIV_FILE_CHOWN_SELF : pchar = 'file_chown_self'+#0;
   PRIV_FILE_DAC_EXECUTE : pchar = 'file_dac_execute'+#0;
   PRIV_FILE_DAC_READ : pchar = 'file_dac_read'+#0;
   PRIV_FILE_DAC_SEARCH : pchar = 'file_dac_search'+#0;
   PRIV_FILE_DAC_WRITE : pchar = 'file_dac_write'+#0;
   PRIV_FILE_DOWNGRADE_SL : pchar = 'file_downgrade_sl'+#0;
   PRIV_FILE_FLAG_SET : pchar = 'file_flag_set'+#0;
   PRIV_FILE_LINK_ANY : pchar = 'file_link_any'+#0;
   PRIV_FILE_OWNER : pchar = 'file_owner'+#0;
   PRIV_FILE_READ : pchar = 'file_read'+#0;
   PRIV_FILE_SETID : pchar = 'file_setid'+#0;
   PRIV_FILE_UPGRADE_SL : pchar = 'file_upgrade_sl'+#0;
   PRIV_FILE_WRITE : pchar = 'file_write'+#0;
   PRIV_GRAPHICS_ACCESS : pchar = 'graphics_access'+#0;
   PRIV_GRAPHICS_MAP : pchar = 'graphics_map'+#0;
   PRIV_IPC_DAC_READ : pchar = 'ipc_dac_read'+#0;
   PRIV_IPC_DAC_WRITE : pchar = 'ipc_dac_write'+#0;
   PRIV_IPC_OWNER : pchar = 'ipc_owner'+#0;
   PRIV_NET_ACCESS : pchar = 'net_access'+#0;
   PRIV_NET_BINDMLP : pchar = 'net_bindmlp'+#0;
   PRIV_NET_ICMPACCESS : pchar = 'net_icmpaccess'+#0;
   PRIV_NET_MAC_AWARE : pchar = 'net_mac_aware'+#0;
   PRIV_NET_MAC_IMPLICIT : pchar = 'net_mac_implicit'+#0;
   PRIV_NET_OBSERVABILITY : pchar = 'net_observability'+#0;
   PRIV_NET_PRIVADDR : pchar = 'net_privaddr'+#0;
   PRIV_NET_RAWACCESS : pchar = 'net_rawaccess'+#0;
   PRIV_PROC_AUDIT : pchar = 'proc_audit'+#0;
   PRIV_PROC_CHROOT : pchar = 'proc_chroot'+#0;
   PRIV_PROC_CLOCK_HIGHRES : pchar = 'proc_clock_highres'+#0;
   PRIV_PROC_EXEC : pchar = 'proc_exec'+#0;
   PRIV_PROC_FORK : pchar = 'proc_fork'+#0;
   PRIV_PROC_INFO : pchar = 'proc_info'+#0;
   PRIV_PROC_LOCK_MEMORY : pchar = 'proc_lock_memory'+#0;
   PRIV_PROC_OWNER : pchar = 'proc_owner'+#0;
   PRIV_PROC_PRIOUP : pchar = 'proc_prioup'+#0;
   PRIV_PROC_PRIOCNTL : pchar = 'proc_priocntl'+#0;
   PRIV_PROC_SESSION : pchar = 'proc_session'+#0;
   PRIV_PROC_SETID : pchar = 'proc_setid'+#0;
   PRIV_PROC_TASKID : pchar = 'proc_taskid'+#0;
   PRIV_PROC_ZONE : pchar = 'proc_zone'+#0;
   PRIV_SYS_ACCT : pchar = 'sys_acct'+#0;
   PRIV_SYS_ADMIN : pchar = 'sys_admin'+#0;
   PRIV_SYS_AUDIT : pchar = 'sys_audit'+#0;
   PRIV_SYS_CONFIG : pchar = 'sys_config'+#0;
   PRIV_SYS_DEVICES : pchar = 'sys_devices'+#0;
   PRIV_SYS_IPC_CONFIG : pchar = 'sys_ipc_config'+#0;
   PRIV_SYS_LINKDIR : pchar = 'sys_linkdir'+#0;
   PRIV_SYS_MOUNT : pchar = 'sys_mount'+#0;
   PRIV_SYS_IPTUN_CONFIG : pchar = 'sys_iptun_config'+#0;
   PRIV_SYS_DL_CONFIG : pchar = 'sys_dl_config'+#0;
   PRIV_SYS_IP_CONFIG : pchar = 'sys_ip_config'+#0;
   PRIV_SYS_NET_CONFIG : pchar = 'sys_net_config'+#0;
   PRIV_SYS_NFS : pchar = 'sys_nfs'+#0;
   PRIV_SYS_PPP_CONFIG : pchar = 'sys_ppp_config'+#0;
   PRIV_SYS_RES_BIND : pchar = 'sys_res_bind'+#0;
   PRIV_SYS_RES_CONFIG : pchar = 'sys_res_config'+#0;
   PRIV_SYS_RESOURCE : pchar = 'sys_resource'+#0;
   PRIV_SYS_SMB : pchar = 'sys_smb'+#0;
   PRIV_SYS_SUSER_COMPAT : pchar = 'sys_suser_compat'+#0;
   PRIV_SYS_TIME : pchar = 'sys_time'+#0;
   PRIV_SYS_TRANS_LABEL : pchar = 'sys_trans_label'+#0;
   PRIV_VIRT_MANAGE : pchar = 'virt_manage'+#0;
   PRIV_WIN_COLORMAP : pchar = 'win_colormap'+#0;
   PRIV_WIN_CONFIG : pchar = 'win_config'+#0;
   PRIV_WIN_DAC_READ : pchar = 'win_dac_read'+#0;
   PRIV_WIN_DAC_WRITE : pchar = 'win_dac_write'+#0;
   PRIV_WIN_DEVICES : pchar = 'win_devices'+#0;
   PRIV_WIN_DGA : pchar = 'win_dga'+#0;
   PRIV_WIN_DOWNGRADE_SL : pchar = 'win_downgrade_sl'+#0;
   PRIV_WIN_FONTPATH : pchar = 'win_fontpath'+#0;
   PRIV_WIN_MAC_READ : pchar = 'win_mac_read'+#0;
   PRIV_WIN_MAC_WRITE : pchar = 'win_mac_write'+#0;
   PRIV_WIN_SELECTION : pchar = 'win_selection'+#0;
   PRIV_WIN_UPGRADE_SL : pchar = 'win_upgrade_sl'+#0;
   PRIV_XVM_CONTROL : pchar = 'xvm_control'+#0;
   PRIV_EFFECTIVE : pchar = 'Effective'+#0;
   PRIV_INHERITABLE : pchar = 'Inheritable'+#0;
   PRIV_PERMITTED : pchar = 'Permitted'+#0;
   PRIV_LIMIT : pchar = 'Limit'+#0;


implementation


end.
