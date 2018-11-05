SET ANSI_NULLS ON
GO
SET QUOTED_IDENTIFIER ON
GO
IF NOT EXISTS (SELECT * FROM sys.objects WHERE object_id = OBJECT_ID(N'[dbo].[last]') AND type in (N'U'))
BEGIN
CREATE TABLE [dbo].[last](
	[username] [varchar](250) COLLATE SQL_Latin1_General_CP1_CI_AS NOT NULL,
	[seconds] [int] NOT NULL,
	[state] [varchar](max) COLLATE SQL_Latin1_General_CP1_CI_AS NOT NULL,
 CONSTRAINT [PK_last_username] PRIMARY KEY CLUSTERED 
(
	[username] ASC
)WITH (STATISTICS_NORECOMPUTE = OFF, IGNORE_DUP_KEY = OFF)
)
END
GO
IF NOT EXISTS (SELECT * FROM sys.indexes WHERE object_id = OBJECT_ID(N'[dbo].[last]') AND name = N'i_last_seconds')
CREATE NONCLUSTERED INDEX [i_last_seconds] ON [dbo].[last]
(
	[seconds] ASC
)WITH (STATISTICS_NORECOMPUTE = OFF, DROP_EXISTING = OFF, ONLINE = OFF)
GO
--~Changing index [dbo].[mam_config].i_mam_config to a clustered index.  You may want to pick a different index to cluster on.
SET ANSI_NULLS ON
GO
SET QUOTED_IDENTIFIER ON
GO
IF NOT EXISTS (SELECT * FROM sys.objects WHERE object_id = OBJECT_ID(N'[dbo].[mam_config]') AND type in (N'U'))
BEGIN
CREATE TABLE [dbo].[mam_config](
	[user_id] [bigint] NOT NULL,
	[remote_jid] [varchar](250) COLLATE SQL_Latin1_General_CP1_CI_AS NOT NULL,
	[behaviour] [varchar](1) COLLATE SQL_Latin1_General_CP1_CI_AS NOT NULL
)
END
GO
SET ANSI_PADDING ON
GO
IF NOT EXISTS (SELECT * FROM sys.indexes WHERE object_id = OBJECT_ID(N'[dbo].[mam_config]') AND name = N'i_mam_config')
CREATE CLUSTERED INDEX [i_mam_config] ON [dbo].[mam_config]
(
	[user_id] ASC,
	[remote_jid] ASC
)WITH (STATISTICS_NORECOMPUTE = OFF, DROP_EXISTING = OFF, ONLINE = OFF)
GO
SET ANSI_NULLS ON
GO
SET QUOTED_IDENTIFIER ON
GO
IF NOT EXISTS (SELECT * FROM sys.objects WHERE object_id = OBJECT_ID(N'[dbo].[mam_message]') AND type in (N'U'))
BEGIN
CREATE TABLE [dbo].[mam_message](
	[id] [bigint] NOT NULL,
	[user_id] [bigint] NOT NULL,
	[from_jid] [varchar](250) COLLATE SQL_Latin1_General_CP1_CI_AS NOT NULL,
	[remote_bare_jid] [varchar](250) COLLATE SQL_Latin1_General_CP1_CI_AS NOT NULL,
	[remote_resource] [varchar](250) COLLATE SQL_Latin1_General_CP1_CI_AS NOT NULL,
	[direction] [varchar](1) COLLATE SQL_Latin1_General_CP1_CI_AS NOT NULL,
	[message] [varchar](max) COLLATE SQL_Latin1_General_CP1_CI_AS NOT NULL,
 CONSTRAINT [PK_mam_message_user_id] PRIMARY KEY CLUSTERED 
(
	[user_id] ASC,
	[id] ASC
)WITH (STATISTICS_NORECOMPUTE = OFF, IGNORE_DUP_KEY = OFF)
)
END
GO
SET ANSI_PADDING ON
GO
IF NOT EXISTS (SELECT * FROM sys.indexes WHERE object_id = OBJECT_ID(N'[dbo].[mam_message]') AND name = N'i_mam_message_rem')
CREATE NONCLUSTERED INDEX [i_mam_message_rem] ON [dbo].[mam_message]
(
	[user_id] ASC,
	[remote_bare_jid] ASC,
	[id] ASC
)WITH (STATISTICS_NORECOMPUTE = OFF, DROP_EXISTING = OFF, ONLINE = OFF)
GO
IF NOT EXISTS (SELECT * FROM sys.indexes WHERE object_id = OBJECT_ID(N'[dbo].[mam_message]') AND name = N'i_mam_message_uid')
CREATE NONCLUSTERED INDEX [i_mam_message_uid] ON [dbo].[mam_message]
(
	[user_id] ASC,
	[id] ASC
)WITH (STATISTICS_NORECOMPUTE = OFF, DROP_EXISTING = OFF, ONLINE = OFF)
GO
SET ANSI_NULLS ON
GO
SET QUOTED_IDENTIFIER ON
GO
IF NOT EXISTS (SELECT * FROM sys.objects WHERE object_id = OBJECT_ID(N'[dbo].[mam_muc_message]') AND type in (N'U'))
BEGIN
CREATE TABLE [dbo].[mam_muc_message](
	[id] [bigint] NOT NULL,
	[room_id] [bigint] NOT NULL,
	[nick_name] [varchar](250) COLLATE SQL_Latin1_General_CP1_CI_AS NOT NULL,
	[message] [varchar](max) COLLATE SQL_Latin1_General_CP1_CI_AS NOT NULL,
 CONSTRAINT [PK_mam_muc_message_id] PRIMARY KEY CLUSTERED 
(
	[id] ASC
)WITH (STATISTICS_NORECOMPUTE = OFF, IGNORE_DUP_KEY = OFF)
)
END
GO
IF NOT EXISTS (SELECT * FROM sys.indexes WHERE object_id = OBJECT_ID(N'[dbo].[mam_muc_message]') AND name = N'i_mam_muc_message_room_name_added_at')
CREATE NONCLUSTERED INDEX [i_mam_muc_message_room_name_added_at] ON [dbo].[mam_muc_message]
(
	[room_id] ASC,
	[id] ASC
)WITH (STATISTICS_NORECOMPUTE = OFF, DROP_EXISTING = OFF, ONLINE = OFF)
GO
SET ANSI_NULLS ON
GO
SET QUOTED_IDENTIFIER ON
GO
IF NOT EXISTS (SELECT * FROM sys.objects WHERE object_id = OBJECT_ID(N'[dbo].[mam_server_user]') AND type in (N'U'))
BEGIN
CREATE TABLE [dbo].[mam_server_user](
	[id] [bigint] IDENTITY(1,1) NOT NULL,
	[server] [varchar](250) COLLATE SQL_Latin1_General_CP1_CI_AS NOT NULL,
	[user_name] [varchar](250) COLLATE SQL_Latin1_General_CP1_CI_AS NOT NULL,
 CONSTRAINT [PK_mam_server_user_id] PRIMARY KEY CLUSTERED 
(
	[id] ASC
)WITH (STATISTICS_NORECOMPUTE = OFF, IGNORE_DUP_KEY = OFF),
 CONSTRAINT [mam_server_user$uc_mam_server_user_name] UNIQUE NONCLUSTERED 
(
	[server] ASC,
	[user_name] ASC
)WITH (STATISTICS_NORECOMPUTE = OFF, IGNORE_DUP_KEY = OFF)
)
END
GO
SET ANSI_PADDING ON
GO
IF NOT EXISTS (SELECT * FROM sys.indexes WHERE object_id = OBJECT_ID(N'[dbo].[mam_server_user]') AND name = N'i_mam_server_user_name')
CREATE NONCLUSTERED INDEX [i_mam_server_user_name] ON [dbo].[mam_server_user]
(
	[server] ASC,
	[user_name] ASC
)WITH (STATISTICS_NORECOMPUTE = OFF, DROP_EXISTING = OFF, ONLINE = OFF)
GO
SET ANSI_NULLS ON
GO
SET QUOTED_IDENTIFIER ON
GO
IF NOT EXISTS (SELECT * FROM sys.objects WHERE object_id = OBJECT_ID(N'[dbo].[mam_user]') AND type in (N'U'))
BEGIN
CREATE TABLE [dbo].[mam_user](
	[id] [bigint] IDENTITY(11,1) NOT NULL,
	[user_name] [varchar](250) COLLATE SQL_Latin1_General_CP1_CI_AS NOT NULL,
 CONSTRAINT [PK_mam_user_id] PRIMARY KEY CLUSTERED 
(
	[id] ASC
)WITH (STATISTICS_NORECOMPUTE = OFF, IGNORE_DUP_KEY = OFF),
 CONSTRAINT [mam_user$uc_mam_user_name] UNIQUE NONCLUSTERED 
(
	[user_name] ASC
)WITH (STATISTICS_NORECOMPUTE = OFF, IGNORE_DUP_KEY = OFF)
)
END
GO
SET ANSI_PADDING ON
GO
IF NOT EXISTS (SELECT * FROM sys.indexes WHERE object_id = OBJECT_ID(N'[dbo].[mam_user]') AND name = N'i_mam_user_name')
CREATE NONCLUSTERED INDEX [i_mam_user_name] ON [dbo].[mam_user]
(
	[user_name] ASC
)WITH (STATISTICS_NORECOMPUTE = OFF, DROP_EXISTING = OFF, ONLINE = OFF)
GO
SET ANSI_NULLS ON
GO
SET QUOTED_IDENTIFIER ON
GO
IF NOT EXISTS (SELECT * FROM sys.objects WHERE object_id = OBJECT_ID(N'[dbo].[offline_message]') AND type in (N'U'))
BEGIN
CREATE TABLE [dbo].[offline_message](
	[id] [bigint] IDENTITY(1,1) NOT NULL,
	[timestamp] [bigint] NOT NULL,
	[expire] [bigint] NULL,
	[server] [varchar](250) COLLATE SQL_Latin1_General_CP1_CI_AS NOT NULL,
	[username] [varchar](250) COLLATE SQL_Latin1_General_CP1_CI_AS NOT NULL,
	[from_jid] [varchar](250) COLLATE SQL_Latin1_General_CP1_CI_AS NOT NULL,
	[packet] [varchar](max) COLLATE SQL_Latin1_General_CP1_CI_AS NOT NULL,
 CONSTRAINT [PK_offline_message_id] PRIMARY KEY CLUSTERED 
(
	[id] ASC
)WITH (STATISTICS_NORECOMPUTE = OFF, IGNORE_DUP_KEY = OFF)
)
END
GO
SET ANSI_PADDING ON
GO
IF NOT EXISTS (SELECT * FROM sys.indexes WHERE object_id = OBJECT_ID(N'[dbo].[offline_message]') AND name = N'i_offline_message')
CREATE NONCLUSTERED INDEX [i_offline_message] ON [dbo].[offline_message]
(
	[server] ASC,
	[username] ASC,
	[id] ASC
)WITH (STATISTICS_NORECOMPUTE = OFF, DROP_EXISTING = OFF, ONLINE = OFF)
GO
IF NOT EXISTS (SELECT * FROM dbo.sysobjects WHERE id = OBJECT_ID(N'[dbo].[DF__offline_m__expir__29221CFB]') AND type = 'D')
BEGIN
ALTER TABLE [dbo].[offline_message] ADD  DEFAULT (NULL) FOR [expire]
END
GO
SET ANSI_NULLS ON
GO
SET QUOTED_IDENTIFIER ON
GO
IF NOT EXISTS (SELECT * FROM sys.objects WHERE object_id = OBJECT_ID(N'[dbo].[privacy_default_list]') AND type in (N'U'))
BEGIN
CREATE TABLE [dbo].[privacy_default_list](
	[username] [varchar](250) COLLATE SQL_Latin1_General_CP1_CI_AS NOT NULL,
	[name] [varchar](250) COLLATE SQL_Latin1_General_CP1_CI_AS NOT NULL,
 CONSTRAINT [PK_privacy_default_list_username] PRIMARY KEY CLUSTERED 
(
	[username] ASC
)WITH (STATISTICS_NORECOMPUTE = OFF, IGNORE_DUP_KEY = OFF)
)
END
GO
SET ANSI_NULLS ON
GO
SET QUOTED_IDENTIFIER ON
GO
IF NOT EXISTS (SELECT * FROM sys.objects WHERE object_id = OBJECT_ID(N'[dbo].[privacy_list]') AND type in (N'U'))
BEGIN
CREATE TABLE [dbo].[privacy_list](
	[username] [varchar](250) COLLATE SQL_Latin1_General_CP1_CI_AS NOT NULL,
	[name] [varchar](250) COLLATE SQL_Latin1_General_CP1_CI_AS NOT NULL,
	[id] [bigint] IDENTITY(1,1) NOT NULL,
	[created_at] [datetime] NOT NULL,
 CONSTRAINT [privacy_list$id] UNIQUE CLUSTERED 
(
	[id] ASC
)WITH (STATISTICS_NORECOMPUTE = OFF, IGNORE_DUP_KEY = OFF)
)
END
GO
SET ANSI_PADDING ON
GO
IF NOT EXISTS (SELECT * FROM sys.indexes WHERE object_id = OBJECT_ID(N'[dbo].[privacy_list]') AND name = N'i_privacy_list_username')
CREATE NONCLUSTERED INDEX [i_privacy_list_username] ON [dbo].[privacy_list]
(
	[username] ASC
)WITH (STATISTICS_NORECOMPUTE = OFF, DROP_EXISTING = OFF, ONLINE = OFF)
GO
IF NOT EXISTS (SELECT * FROM dbo.sysobjects WHERE id = OBJECT_ID(N'[dbo].[DF__privacy_l__creat__0D7A0286]') AND type = 'D')
BEGIN
ALTER TABLE [dbo].[privacy_list] ADD  DEFAULT (getdate()) FOR [created_at]
END
GO
--~Adding clustered index ci_azure_fixup_dbo_privacy_list_data on [dbo].[privacy_list_data].  You may want to change this index.
SET ANSI_NULLS ON
GO
SET QUOTED_IDENTIFIER ON
GO
IF NOT EXISTS (SELECT * FROM sys.objects WHERE object_id = OBJECT_ID(N'[dbo].[privacy_list_data]') AND type in (N'U'))
BEGIN
CREATE TABLE [dbo].[privacy_list_data](
	[id] [bigint] NULL,
	[t] [char](1) COLLATE SQL_Latin1_General_CP1_CI_AS NOT NULL,
	[value] [varchar](max) COLLATE SQL_Latin1_General_CP1_CI_AS NOT NULL,
	[action] [char](1) COLLATE SQL_Latin1_General_CP1_CI_AS NOT NULL,
	[ord] [bigint] NOT NULL,
	[match_all] [smallint] NOT NULL,
	[match_iq] [smallint] NOT NULL,
	[match_message] [smallint] NOT NULL,
	[match_presence_in] [smallint] NOT NULL,
	[match_presence_out] [smallint] NOT NULL
)
END
GO
IF NOT EXISTS (SELECT * FROM sys.indexes WHERE object_id = OBJECT_ID(N'[dbo].[privacy_list_data]') AND name = N'ci_azure_fixup_dbo_privacy_list_data')
CREATE CLUSTERED INDEX [ci_azure_fixup_dbo_privacy_list_data] ON [dbo].[privacy_list_data]
(
	[id]
)WITH (DROP_EXISTING = OFF, ONLINE = OFF)
GO
IF NOT EXISTS (SELECT * FROM dbo.sysobjects WHERE id = OBJECT_ID(N'[dbo].[DF__privacy_list__id__0E6E26BF]') AND type = 'D')
BEGIN
ALTER TABLE [dbo].[privacy_list_data] ADD  DEFAULT (NULL) FOR [id]
END
GO
SET ANSI_NULLS ON
GO
SET QUOTED_IDENTIFIER ON
GO
IF NOT EXISTS (SELECT * FROM sys.objects WHERE object_id = OBJECT_ID(N'[dbo].[private_storage]') AND type in (N'U'))
BEGIN
CREATE TABLE [dbo].[private_storage](
	[username] [varchar](250) COLLATE SQL_Latin1_General_CP1_CI_AS NOT NULL,
	[namespace] [varchar](250) COLLATE SQL_Latin1_General_CP1_CI_AS NOT NULL,
	[data] [varchar](max) COLLATE SQL_Latin1_General_CP1_CI_AS NOT NULL,
	[created_at] [datetime] NOT NULL,
 CONSTRAINT [private_storage$i_private_storage_username_namespace] UNIQUE CLUSTERED 
(
	[username] ASC,
	[namespace] ASC
)WITH (STATISTICS_NORECOMPUTE = OFF, IGNORE_DUP_KEY = OFF)
)
END
GO
SET ANSI_PADDING ON
GO
IF NOT EXISTS (SELECT * FROM sys.indexes WHERE object_id = OBJECT_ID(N'[dbo].[private_storage]') AND name = N'i_private_storage_username')
CREATE NONCLUSTERED INDEX [i_private_storage_username] ON [dbo].[private_storage]
(
	[username] ASC
)WITH (STATISTICS_NORECOMPUTE = OFF, DROP_EXISTING = OFF, ONLINE = OFF)
GO
IF NOT EXISTS (SELECT * FROM dbo.sysobjects WHERE id = OBJECT_ID(N'[dbo].[DF__private_s__creat__0F624AF8]') AND type = 'D')
BEGIN
ALTER TABLE [dbo].[private_storage] ADD  DEFAULT (getdate()) FOR [created_at]
END
GO
SET ANSI_NULLS ON
GO
SET QUOTED_IDENTIFIER ON
GO
IF NOT EXISTS (SELECT * FROM sys.objects WHERE object_id = OBJECT_ID(N'[dbo].[roster_version]') AND type in (N'U'))
BEGIN
CREATE TABLE [dbo].[roster_version](
	[username] [varchar](250) COLLATE SQL_Latin1_General_CP1_CI_AS NOT NULL,
	[version] [varchar](max) COLLATE SQL_Latin1_General_CP1_CI_AS NOT NULL,
 CONSTRAINT [PK_roster_version_username] PRIMARY KEY CLUSTERED 
(
	[username] ASC
)WITH (STATISTICS_NORECOMPUTE = OFF, IGNORE_DUP_KEY = OFF)
)
END
GO
--~Changing index [dbo].[rostergroups].pk_rosterg_user_jid to a clustered index.  You may want to pick a different index to cluster on.
SET ANSI_NULLS ON
GO
SET QUOTED_IDENTIFIER ON
GO
IF NOT EXISTS (SELECT * FROM sys.objects WHERE object_id = OBJECT_ID(N'[dbo].[rostergroups]') AND type in (N'U'))
BEGIN
CREATE TABLE [dbo].[rostergroups](
	[username] [varchar](250) COLLATE SQL_Latin1_General_CP1_CI_AS NOT NULL,
	[jid] [varchar](250) COLLATE SQL_Latin1_General_CP1_CI_AS NOT NULL,
	[grp] [varchar](max) COLLATE SQL_Latin1_General_CP1_CI_AS NOT NULL
)
END
GO
SET ANSI_PADDING ON
GO
IF NOT EXISTS (SELECT * FROM sys.indexes WHERE object_id = OBJECT_ID(N'[dbo].[rostergroups]') AND name = N'pk_rosterg_user_jid')
CREATE CLUSTERED INDEX [pk_rosterg_user_jid] ON [dbo].[rostergroups]
(
	[username] ASC,
	[jid] ASC
)WITH (STATISTICS_NORECOMPUTE = OFF, DROP_EXISTING = OFF, ONLINE = OFF)
GO
SET ANSI_NULLS ON
GO
SET QUOTED_IDENTIFIER ON
GO
IF NOT EXISTS (SELECT * FROM sys.objects WHERE object_id = OBJECT_ID(N'[dbo].[rosterusers]') AND type in (N'U'))
BEGIN
CREATE TABLE [dbo].[rosterusers](
	[username] [varchar](250) COLLATE SQL_Latin1_General_CP1_CI_AS NOT NULL,
	[jid] [varchar](250) COLLATE SQL_Latin1_General_CP1_CI_AS NOT NULL,
	[nick] [varchar](max) COLLATE SQL_Latin1_General_CP1_CI_AS NOT NULL,
	[subscription] [char](1) COLLATE SQL_Latin1_General_CP1_CI_AS NOT NULL,
	[ask] [char](1) COLLATE SQL_Latin1_General_CP1_CI_AS NOT NULL,
	[askmessage] [varchar](max) COLLATE SQL_Latin1_General_CP1_CI_AS NOT NULL,
	[server] [char](1) COLLATE SQL_Latin1_General_CP1_CI_AS NOT NULL,
	[subscribe] [varchar](max) COLLATE SQL_Latin1_General_CP1_CI_AS NOT NULL,
	[type] [varchar](max) COLLATE SQL_Latin1_General_CP1_CI_AS NULL,
	[created_at] [datetime] NOT NULL,
 CONSTRAINT [rosterusers$i_rosteru_user_jid] UNIQUE CLUSTERED 
(
	[username] ASC,
	[jid] ASC
)WITH (STATISTICS_NORECOMPUTE = OFF, IGNORE_DUP_KEY = OFF)
)
END
GO
SET ANSI_PADDING ON
GO
IF NOT EXISTS (SELECT * FROM sys.indexes WHERE object_id = OBJECT_ID(N'[dbo].[rosterusers]') AND name = N'i_rosteru_jid')
CREATE NONCLUSTERED INDEX [i_rosteru_jid] ON [dbo].[rosterusers]
(
	[jid] ASC
)WITH (STATISTICS_NORECOMPUTE = OFF, DROP_EXISTING = OFF, ONLINE = OFF)
GO
SET ANSI_PADDING ON
GO
IF NOT EXISTS (SELECT * FROM sys.indexes WHERE object_id = OBJECT_ID(N'[dbo].[rosterusers]') AND name = N'i_rosteru_username')
CREATE NONCLUSTERED INDEX [i_rosteru_username] ON [dbo].[rosterusers]
(
	[username] ASC
)WITH (STATISTICS_NORECOMPUTE = OFF, DROP_EXISTING = OFF, ONLINE = OFF)
GO
IF NOT EXISTS (SELECT * FROM dbo.sysobjects WHERE id = OBJECT_ID(N'[dbo].[DF__rosteruse__creat__160F4887]') AND type = 'D')
BEGIN
ALTER TABLE [dbo].[rosterusers] ADD  DEFAULT (getdate()) FOR [created_at]
END
GO
SET ANSI_NULLS ON
GO
SET QUOTED_IDENTIFIER ON
GO
IF NOT EXISTS (SELECT * FROM sys.objects WHERE object_id = OBJECT_ID(N'[dbo].[users]') AND type in (N'U'))
BEGIN
CREATE TABLE [dbo].[users](
	[username] [varchar](250) COLLATE SQL_Latin1_General_CP1_CI_AS NOT NULL,
	[password] [varchar](max) COLLATE SQL_Latin1_General_CP1_CI_AS NOT NULL,
	[pass_details] [varchar](max) COLLATE SQL_Latin1_General_CP1_CI_AS NULL,
	[created_at] [datetime] NOT NULL,
 CONSTRAINT [PK_users_username] PRIMARY KEY CLUSTERED 
(
	[username] ASC
)WITH (STATISTICS_NORECOMPUTE = OFF, IGNORE_DUP_KEY = OFF)
)
END
GO
IF NOT EXISTS (SELECT * FROM dbo.sysobjects WHERE id = OBJECT_ID(N'[dbo].[DF__users__created_a__17F790F9]') AND type = 'D')
BEGIN
ALTER TABLE [dbo].[users] ADD  DEFAULT (getdate()) FOR [created_at]
END
GO
SET ANSI_NULLS ON
GO
SET QUOTED_IDENTIFIER ON
GO
IF NOT EXISTS (SELECT * FROM sys.objects WHERE object_id = OBJECT_ID(N'[dbo].[vcard]') AND type in (N'U'))
BEGIN
CREATE TABLE [dbo].[vcard](
	[username] [varchar](150) COLLATE SQL_Latin1_General_CP1_CI_AS NOT NULL,
	[server] [varchar](150) COLLATE SQL_Latin1_General_CP1_CI_AS NOT NULL,
	[vcard] [varchar](max) COLLATE SQL_Latin1_General_CP1_CI_AS NOT NULL,
	[created_at] [datetime] NOT NULL,
 CONSTRAINT [PK_vcard_username] PRIMARY KEY CLUSTERED 
(
	[username] ASC,
	[server] ASC
)WITH (STATISTICS_NORECOMPUTE = OFF, IGNORE_DUP_KEY = OFF)
)
END
GO
IF NOT EXISTS (SELECT * FROM dbo.sysobjects WHERE id = OBJECT_ID(N'[dbo].[DF__vcard__username__18EBB532]') AND type = 'D')
BEGIN
ALTER TABLE [dbo].[vcard] ADD  DEFAULT (N'') FOR [username]
END
GO
IF NOT EXISTS (SELECT * FROM dbo.sysobjects WHERE id = OBJECT_ID(N'[dbo].[DF__vcard__server__19DFD96B]') AND type = 'D')
BEGIN
ALTER TABLE [dbo].[vcard] ADD  DEFAULT (N'') FOR [server]
END
GO
IF NOT EXISTS (SELECT * FROM dbo.sysobjects WHERE id = OBJECT_ID(N'[dbo].[DF__vcard__created_a__1AD3FDA4]') AND type = 'D')
BEGIN
ALTER TABLE [dbo].[vcard] ADD  DEFAULT (getdate()) FOR [created_at]
END
GO
SET ANSI_NULLS ON
GO
SET QUOTED_IDENTIFIER ON
GO
IF NOT EXISTS (SELECT * FROM sys.objects WHERE object_id = OBJECT_ID(N'[dbo].[vcard_search]') AND type in (N'U'))
BEGIN
CREATE TABLE [dbo].[vcard_search](
	[username] [varchar](150) COLLATE SQL_Latin1_General_CP1_CI_AS NOT NULL,
	[lusername] [varchar](100) COLLATE SQL_Latin1_General_CP1_CI_AS NOT NULL,
	[server] [varchar](150) COLLATE SQL_Latin1_General_CP1_CI_AS NOT NULL,
	[fn] [varchar](max) COLLATE SQL_Latin1_General_CP1_CI_AS NOT NULL,
	[lfn] [varchar](250) COLLATE SQL_Latin1_General_CP1_CI_AS NOT NULL,
	[family] [varchar](max) COLLATE SQL_Latin1_General_CP1_CI_AS NOT NULL,
	[lfamily] [varchar](250) COLLATE SQL_Latin1_General_CP1_CI_AS NOT NULL,
	[given] [varchar](max) COLLATE SQL_Latin1_General_CP1_CI_AS NOT NULL,
	[lgiven] [varchar](250) COLLATE SQL_Latin1_General_CP1_CI_AS NOT NULL,
	[middle] [varchar](max) COLLATE SQL_Latin1_General_CP1_CI_AS NOT NULL,
	[lmiddle] [varchar](250) COLLATE SQL_Latin1_General_CP1_CI_AS NOT NULL,
	[nickname] [varchar](max) COLLATE SQL_Latin1_General_CP1_CI_AS NOT NULL,
	[lnickname] [varchar](250) COLLATE SQL_Latin1_General_CP1_CI_AS NOT NULL,
	[bday] [varchar](max) COLLATE SQL_Latin1_General_CP1_CI_AS NOT NULL,
	[lbday] [varchar](250) COLLATE SQL_Latin1_General_CP1_CI_AS NOT NULL,
	[ctry] [varchar](max) COLLATE SQL_Latin1_General_CP1_CI_AS NOT NULL,
	[lctry] [varchar](250) COLLATE SQL_Latin1_General_CP1_CI_AS NOT NULL,
	[locality] [varchar](max) COLLATE SQL_Latin1_General_CP1_CI_AS NOT NULL,
	[llocality] [varchar](250) COLLATE SQL_Latin1_General_CP1_CI_AS NOT NULL,
	[email] [varchar](max) COLLATE SQL_Latin1_General_CP1_CI_AS NOT NULL,
	[lemail] [varchar](250) COLLATE SQL_Latin1_General_CP1_CI_AS NOT NULL,
	[orgname] [varchar](max) COLLATE SQL_Latin1_General_CP1_CI_AS NOT NULL,
	[lorgname] [varchar](250) COLLATE SQL_Latin1_General_CP1_CI_AS NOT NULL,
	[orgunit] [varchar](max) COLLATE SQL_Latin1_General_CP1_CI_AS NOT NULL,
	[lorgunit] [varchar](250) COLLATE SQL_Latin1_General_CP1_CI_AS NOT NULL,
 CONSTRAINT [PK_vcard_search_lusername] PRIMARY KEY CLUSTERED 
(
	[lusername] ASC,
	[server] ASC
)WITH (STATISTICS_NORECOMPUTE = OFF, IGNORE_DUP_KEY = OFF)
)
END
GO
SET ANSI_PADDING ON
GO
IF NOT EXISTS (SELECT * FROM sys.indexes WHERE object_id = OBJECT_ID(N'[dbo].[vcard_search]') AND name = N'i_vcard_search_lbday')
CREATE NONCLUSTERED INDEX [i_vcard_search_lbday] ON [dbo].[vcard_search]
(
	[lbday] ASC
)WITH (STATISTICS_NORECOMPUTE = OFF, DROP_EXISTING = OFF, ONLINE = OFF)
GO
SET ANSI_PADDING ON
GO
IF NOT EXISTS (SELECT * FROM sys.indexes WHERE object_id = OBJECT_ID(N'[dbo].[vcard_search]') AND name = N'i_vcard_search_lctry')
CREATE NONCLUSTERED INDEX [i_vcard_search_lctry] ON [dbo].[vcard_search]
(
	[lctry] ASC
)WITH (STATISTICS_NORECOMPUTE = OFF, DROP_EXISTING = OFF, ONLINE = OFF)
GO
SET ANSI_PADDING ON
GO
IF NOT EXISTS (SELECT * FROM sys.indexes WHERE object_id = OBJECT_ID(N'[dbo].[vcard_search]') AND name = N'i_vcard_search_lemail')
CREATE NONCLUSTERED INDEX [i_vcard_search_lemail] ON [dbo].[vcard_search]
(
	[lemail] ASC
)WITH (STATISTICS_NORECOMPUTE = OFF, DROP_EXISTING = OFF, ONLINE = OFF)
GO
SET ANSI_PADDING ON
GO
IF NOT EXISTS (SELECT * FROM sys.indexes WHERE object_id = OBJECT_ID(N'[dbo].[vcard_search]') AND name = N'i_vcard_search_lfamily')
CREATE NONCLUSTERED INDEX [i_vcard_search_lfamily] ON [dbo].[vcard_search]
(
	[lfamily] ASC
)WITH (STATISTICS_NORECOMPUTE = OFF, DROP_EXISTING = OFF, ONLINE = OFF)
GO
SET ANSI_PADDING ON
GO
IF NOT EXISTS (SELECT * FROM sys.indexes WHERE object_id = OBJECT_ID(N'[dbo].[vcard_search]') AND name = N'i_vcard_search_lfn')
CREATE NONCLUSTERED INDEX [i_vcard_search_lfn] ON [dbo].[vcard_search]
(
	[lfn] ASC
)WITH (STATISTICS_NORECOMPUTE = OFF, DROP_EXISTING = OFF, ONLINE = OFF)
GO
SET ANSI_PADDING ON
GO
IF NOT EXISTS (SELECT * FROM sys.indexes WHERE object_id = OBJECT_ID(N'[dbo].[vcard_search]') AND name = N'i_vcard_search_lgiven')
CREATE NONCLUSTERED INDEX [i_vcard_search_lgiven] ON [dbo].[vcard_search]
(
	[lgiven] ASC
)WITH (STATISTICS_NORECOMPUTE = OFF, DROP_EXISTING = OFF, ONLINE = OFF)
GO
SET ANSI_PADDING ON
GO
IF NOT EXISTS (SELECT * FROM sys.indexes WHERE object_id = OBJECT_ID(N'[dbo].[vcard_search]') AND name = N'i_vcard_search_llocality')
CREATE NONCLUSTERED INDEX [i_vcard_search_llocality] ON [dbo].[vcard_search]
(
	[llocality] ASC
)WITH (STATISTICS_NORECOMPUTE = OFF, DROP_EXISTING = OFF, ONLINE = OFF)
GO
SET ANSI_PADDING ON
GO
IF NOT EXISTS (SELECT * FROM sys.indexes WHERE object_id = OBJECT_ID(N'[dbo].[vcard_search]') AND name = N'i_vcard_search_lmiddle')
CREATE NONCLUSTERED INDEX [i_vcard_search_lmiddle] ON [dbo].[vcard_search]
(
	[lmiddle] ASC
)WITH (STATISTICS_NORECOMPUTE = OFF, DROP_EXISTING = OFF, ONLINE = OFF)
GO
SET ANSI_PADDING ON
GO
IF NOT EXISTS (SELECT * FROM sys.indexes WHERE object_id = OBJECT_ID(N'[dbo].[vcard_search]') AND name = N'i_vcard_search_lnickname')
CREATE NONCLUSTERED INDEX [i_vcard_search_lnickname] ON [dbo].[vcard_search]
(
	[lnickname] ASC
)WITH (STATISTICS_NORECOMPUTE = OFF, DROP_EXISTING = OFF, ONLINE = OFF)
GO
SET ANSI_PADDING ON
GO
IF NOT EXISTS (SELECT * FROM sys.indexes WHERE object_id = OBJECT_ID(N'[dbo].[vcard_search]') AND name = N'i_vcard_search_lorgname')
CREATE NONCLUSTERED INDEX [i_vcard_search_lorgname] ON [dbo].[vcard_search]
(
	[lorgname] ASC
)WITH (STATISTICS_NORECOMPUTE = OFF, DROP_EXISTING = OFF, ONLINE = OFF)
GO
SET ANSI_PADDING ON
GO
IF NOT EXISTS (SELECT * FROM sys.indexes WHERE object_id = OBJECT_ID(N'[dbo].[vcard_search]') AND name = N'i_vcard_search_lorgunit')
CREATE NONCLUSTERED INDEX [i_vcard_search_lorgunit] ON [dbo].[vcard_search]
(
	[lorgunit] ASC
)WITH (STATISTICS_NORECOMPUTE = OFF, DROP_EXISTING = OFF, ONLINE = OFF)
GO
SET ANSI_PADDING ON
GO
IF NOT EXISTS (SELECT * FROM sys.indexes WHERE object_id = OBJECT_ID(N'[dbo].[vcard_search]') AND name = N'i_vcard_search_server')
CREATE NONCLUSTERED INDEX [i_vcard_search_server] ON [dbo].[vcard_search]
(
	[server] ASC
)WITH (STATISTICS_NORECOMPUTE = OFF, DROP_EXISTING = OFF, ONLINE = OFF)
GO
IF NOT EXISTS (SELECT * FROM dbo.sysobjects WHERE id = OBJECT_ID(N'[dbo].[DF__vcard_sea__luser__1BC821DD]') AND type = 'D')
BEGIN
ALTER TABLE [dbo].[vcard_search] ADD  DEFAULT (N'') FOR [lusername]
END
GO
IF NOT EXISTS (SELECT * FROM dbo.sysobjects WHERE id = OBJECT_ID(N'[dbo].[DF__vcard_sea__serve__1CBC4616]') AND type = 'D')
BEGIN
ALTER TABLE [dbo].[vcard_search] ADD  DEFAULT (N'') FOR [server]
END
GO
-- BCPArgs:29:[dbo].[mam_message] in "c:\SQLAzureMW\BCPData\dbo.mam_message.dat" -E -n -C RAW -b 10000 -a 16384
GO
-- BCPArgs:15:[dbo].[mam_muc_message] in "c:\SQLAzureMW\BCPData\dbo.mam_muc_message.dat" -E -n -C RAW -b 10000 -a 16384
GO
-- BCPArgs:11:[dbo].[mam_user] in "c:\SQLAzureMW\BCPData\dbo.mam_user.dat" -E -n -C RAW -b 10000 -a 16384
GO

