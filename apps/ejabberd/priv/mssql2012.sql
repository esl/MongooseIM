USE [ejabberd]
GO
/****** Object:  Table [dbo].[last]    Script Date: 9/17/2014 6:20:03 AM ******/
SET ANSI_NULLS ON
GO
SET QUOTED_IDENTIFIER ON
GO
SET ANSI_PADDING ON
GO
CREATE TABLE [dbo].[last](
	[username] [varchar](250) NOT NULL,
	[seconds] [int] NOT NULL,
	[state] [varchar](max) NOT NULL,
 CONSTRAINT [PK_last_username] PRIMARY KEY CLUSTERED
(
	[username] ASC
)WITH (PAD_INDEX = OFF, STATISTICS_NORECOMPUTE = OFF, IGNORE_DUP_KEY = OFF, ALLOW_ROW_LOCKS = ON, ALLOW_PAGE_LOCKS = ON) ON [PRIMARY]
) ON [PRIMARY] TEXTIMAGE_ON [PRIMARY]

GO
SET ANSI_PADDING OFF
GO
/****** Object:  Table [dbo].[mam_config]    Script Date: 9/17/2014 6:20:03 AM ******/
SET ANSI_NULLS ON
GO
SET QUOTED_IDENTIFIER ON
GO
SET ANSI_PADDING ON
GO
CREATE TABLE [dbo].[mam_config](
	[user_id] [bigint] NOT NULL,
	[remote_jid] [varchar](250) NOT NULL,
	[behaviour] [varchar](1) NOT NULL
) ON [PRIMARY]

GO
SET ANSI_PADDING OFF
GO
/****** Object:  Table [dbo].[mam_message]    Script Date: 9/17/2014 6:20:03 AM ******/
SET ANSI_NULLS ON
GO
SET QUOTED_IDENTIFIER ON
GO
SET ANSI_PADDING ON
GO
CREATE TABLE [dbo].[mam_message](
	[id] [bigint] NOT NULL,
	[user_id] [bigint] NOT NULL,
	[from_jid] [varchar](250) NOT NULL,
	[remote_bare_jid] [varchar](250) NOT NULL,
	[remote_resource] [varchar](250) NOT NULL,
	[direction] [varchar](1) NOT NULL,
	[message] [varchar](max) NOT NULL,
 CONSTRAINT [PK_mam_message_user_id] PRIMARY KEY CLUSTERED
(
	[user_id] ASC,
	[id] ASC
)WITH (PAD_INDEX = OFF, STATISTICS_NORECOMPUTE = OFF, IGNORE_DUP_KEY = OFF, ALLOW_ROW_LOCKS = ON, ALLOW_PAGE_LOCKS = ON) ON [PRIMARY]
) ON [PRIMARY] TEXTIMAGE_ON [PRIMARY]

GO
SET ANSI_PADDING OFF
GO
/****** Object:  Table [dbo].[mam_muc_message]    Script Date: 9/17/2014 6:20:03 AM ******/
SET ANSI_NULLS ON
GO
SET QUOTED_IDENTIFIER ON
GO
SET ANSI_PADDING ON
GO
CREATE TABLE [dbo].[mam_muc_message](
	[id] [bigint] NOT NULL,
	[room_id] [bigint] NOT NULL,
	[nick_name] [varchar](250) NOT NULL,
	[message] [varchar](max) NOT NULL,
 CONSTRAINT [PK_mam_muc_message_id] PRIMARY KEY CLUSTERED
(
	[id] ASC
)WITH (PAD_INDEX = OFF, STATISTICS_NORECOMPUTE = OFF, IGNORE_DUP_KEY = OFF, ALLOW_ROW_LOCKS = ON, ALLOW_PAGE_LOCKS = ON) ON [PRIMARY]
) ON [PRIMARY] TEXTIMAGE_ON [PRIMARY]

GO
SET ANSI_PADDING OFF
GO
/****** Object:  Table [dbo].[mam_server_user]    Script Date: 9/17/2014 6:20:03 AM ******/
SET ANSI_NULLS ON
GO
SET QUOTED_IDENTIFIER ON
GO
SET ANSI_PADDING ON
GO
CREATE TABLE [dbo].[mam_server_user](
	[id] [bigint] IDENTITY(1,1) NOT NULL,
	[server] [varchar](250) NOT NULL,
	[user_name] [varchar](250) NOT NULL,
 CONSTRAINT [PK_mam_server_user_id] PRIMARY KEY CLUSTERED
(
	[id] ASC
)WITH (PAD_INDEX = OFF, STATISTICS_NORECOMPUTE = OFF, IGNORE_DUP_KEY = OFF, ALLOW_ROW_LOCKS = ON, ALLOW_PAGE_LOCKS = ON) ON [PRIMARY],
 CONSTRAINT [mam_server_user$uc_mam_server_user_name] UNIQUE NONCLUSTERED
(
	[server] ASC,
	[user_name] ASC
)WITH (PAD_INDEX = OFF, STATISTICS_NORECOMPUTE = OFF, IGNORE_DUP_KEY = OFF, ALLOW_ROW_LOCKS = ON, ALLOW_PAGE_LOCKS = ON) ON [PRIMARY]
) ON [PRIMARY]

GO
SET ANSI_PADDING OFF
GO
/****** Object:  Table [dbo].[mam_user]    Script Date: 9/17/2014 6:20:03 AM ******/
SET ANSI_NULLS ON
GO
SET QUOTED_IDENTIFIER ON
GO
SET ANSI_PADDING ON
GO
CREATE TABLE [dbo].[mam_user](
	[id] [bigint] IDENTITY(11,1) NOT NULL,
	[user_name] [varchar](250) NOT NULL,
 CONSTRAINT [PK_mam_user_id] PRIMARY KEY CLUSTERED
(
	[id] ASC
)WITH (PAD_INDEX = OFF, STATISTICS_NORECOMPUTE = OFF, IGNORE_DUP_KEY = OFF, ALLOW_ROW_LOCKS = ON, ALLOW_PAGE_LOCKS = ON) ON [PRIMARY],
 CONSTRAINT [mam_user$uc_mam_user_name] UNIQUE NONCLUSTERED
(
	[user_name] ASC
)WITH (PAD_INDEX = OFF, STATISTICS_NORECOMPUTE = OFF, IGNORE_DUP_KEY = OFF, ALLOW_ROW_LOCKS = ON, ALLOW_PAGE_LOCKS = ON) ON [PRIMARY]
) ON [PRIMARY]

GO
SET ANSI_PADDING OFF
GO
/****** Object:  Table [dbo].[offline_message]    Script Date: 9/17/2014 6:20:03 AM ******/
SET ANSI_NULLS ON
GO
SET QUOTED_IDENTIFIER ON
GO
SET ANSI_PADDING ON
GO
CREATE TABLE [dbo].[offline_message](
	[id] [bigint] IDENTITY(1,1) NOT NULL,
	[timestamp] [bigint] NOT NULL,
	[expire] [bigint] NULL,
	[server] [varchar](250) NOT NULL,
	[username] [varchar](250) NOT NULL,
	[from_jid] [varchar](250) NOT NULL,
	[packet] [varchar](max) NOT NULL,
 CONSTRAINT [PK_offline_message_id] PRIMARY KEY CLUSTERED
(
	[id] ASC
)WITH (PAD_INDEX = OFF, STATISTICS_NORECOMPUTE = OFF, IGNORE_DUP_KEY = OFF, ALLOW_ROW_LOCKS = ON, ALLOW_PAGE_LOCKS = ON) ON [PRIMARY]
) ON [PRIMARY] TEXTIMAGE_ON [PRIMARY]

GO
SET ANSI_PADDING OFF
GO
/****** Object:  Table [dbo].[privacy_default_list]    Script Date: 9/17/2014 6:20:03 AM ******/
SET ANSI_NULLS ON
GO
SET QUOTED_IDENTIFIER ON
GO
SET ANSI_PADDING ON
GO
CREATE TABLE [dbo].[privacy_default_list](
	[username] [varchar](250) NOT NULL,
	[name] [varchar](250) NOT NULL,
 CONSTRAINT [PK_privacy_default_list_username] PRIMARY KEY CLUSTERED
(
	[username] ASC
)WITH (PAD_INDEX = OFF, STATISTICS_NORECOMPUTE = OFF, IGNORE_DUP_KEY = OFF, ALLOW_ROW_LOCKS = ON, ALLOW_PAGE_LOCKS = ON) ON [PRIMARY]
) ON [PRIMARY]

GO
SET ANSI_PADDING OFF
GO
/****** Object:  Table [dbo].[privacy_list]    Script Date: 9/17/2014 6:20:03 AM ******/
SET ANSI_NULLS ON
GO
SET QUOTED_IDENTIFIER ON
GO
SET ANSI_PADDING ON
GO
CREATE TABLE [dbo].[privacy_list](
	[username] [varchar](250) NOT NULL,
	[name] [varchar](250) NOT NULL,
	[id] [bigint] IDENTITY(1,1) NOT NULL,
	[created_at] [datetime] NOT NULL,
 CONSTRAINT [privacy_list$id] UNIQUE CLUSTERED
(
	[id] ASC
)WITH (PAD_INDEX = OFF, STATISTICS_NORECOMPUTE = OFF, IGNORE_DUP_KEY = OFF, ALLOW_ROW_LOCKS = ON, ALLOW_PAGE_LOCKS = ON) ON [PRIMARY]
) ON [PRIMARY]

GO
SET ANSI_PADDING OFF
GO
/****** Object:  Table [dbo].[privacy_list_data]    Script Date: 9/17/2014 6:20:03 AM ******/
SET ANSI_NULLS ON
GO
SET QUOTED_IDENTIFIER ON
GO
SET ANSI_PADDING ON
GO
CREATE TABLE [dbo].[privacy_list_data](
	[id] [bigint] NULL,
	[t] [char](1) NOT NULL,
	[value] [varchar](max) NOT NULL,
	[action] [char](1) NOT NULL,
	[ord] [bigint] NOT NULL,
	[match_all] [smallint] NOT NULL,
	[match_iq] [smallint] NOT NULL,
	[match_message] [smallint] NOT NULL,
	[match_presence_in] [smallint] NOT NULL,
	[match_presence_out] [smallint] NOT NULL
) ON [PRIMARY] TEXTIMAGE_ON [PRIMARY]

GO
SET ANSI_PADDING OFF
GO
/****** Object:  Table [dbo].[private_storage]    Script Date: 9/17/2014 6:20:03 AM ******/
SET ANSI_NULLS ON
GO
SET QUOTED_IDENTIFIER ON
GO
SET ANSI_PADDING ON
GO
CREATE TABLE [dbo].[private_storage](
	[username] [varchar](250) NOT NULL,
	[namespace] [varchar](250) NOT NULL,
	[data] [varchar](max) NOT NULL,
	[created_at] [datetime] NOT NULL,
 CONSTRAINT [private_storage$i_private_storage_username_namespace] UNIQUE CLUSTERED
(
	[username] ASC,
	[namespace] ASC
)WITH (PAD_INDEX = OFF, STATISTICS_NORECOMPUTE = OFF, IGNORE_DUP_KEY = OFF, ALLOW_ROW_LOCKS = ON, ALLOW_PAGE_LOCKS = ON) ON [PRIMARY]
) ON [PRIMARY] TEXTIMAGE_ON [PRIMARY]

GO
SET ANSI_PADDING OFF
GO
/****** Object:  Table [dbo].[roster_version]    Script Date: 9/17/2014 6:20:03 AM ******/
SET ANSI_NULLS ON
GO
SET QUOTED_IDENTIFIER ON
GO
SET ANSI_PADDING ON
GO
CREATE TABLE [dbo].[roster_version](
	[username] [varchar](250) NOT NULL,
	[version] [varchar](max) NOT NULL,
 CONSTRAINT [PK_roster_version_username] PRIMARY KEY CLUSTERED
(
	[username] ASC
)WITH (PAD_INDEX = OFF, STATISTICS_NORECOMPUTE = OFF, IGNORE_DUP_KEY = OFF, ALLOW_ROW_LOCKS = ON, ALLOW_PAGE_LOCKS = ON) ON [PRIMARY]
) ON [PRIMARY] TEXTIMAGE_ON [PRIMARY]

GO
SET ANSI_PADDING OFF
GO
/****** Object:  Table [dbo].[rostergroups]    Script Date: 9/17/2014 6:20:03 AM ******/
SET ANSI_NULLS ON
GO
SET QUOTED_IDENTIFIER ON
GO
SET ANSI_PADDING ON
GO
CREATE TABLE [dbo].[rostergroups](
	[username] [varchar](250) NOT NULL,
	[jid] [varchar](250) NOT NULL,
	[grp] [varchar](max) NOT NULL
) ON [PRIMARY] TEXTIMAGE_ON [PRIMARY]

GO
SET ANSI_PADDING OFF
GO
/****** Object:  Table [dbo].[rosterusers]    Script Date: 9/17/2014 6:20:03 AM ******/
SET ANSI_NULLS ON
GO
SET QUOTED_IDENTIFIER ON
GO
SET ANSI_PADDING ON
GO
CREATE TABLE [dbo].[rosterusers](
	[username] [varchar](250) NOT NULL,
	[jid] [varchar](250) NOT NULL,
	[nick] [varchar](max) NOT NULL,
	[subscription] [char](1) NOT NULL,
	[ask] [char](1) NOT NULL,
	[askmessage] [varchar](max) NOT NULL,
	[server] [char](1) NOT NULL,
	[subscribe] [varchar](max) NOT NULL,
	[type] [varchar](max) NULL,
	[created_at] [datetime] NOT NULL,
 CONSTRAINT [rosterusers$i_rosteru_user_jid] UNIQUE CLUSTERED
(
	[username] ASC,
	[jid] ASC
)WITH (PAD_INDEX = OFF, STATISTICS_NORECOMPUTE = OFF, IGNORE_DUP_KEY = OFF, ALLOW_ROW_LOCKS = ON, ALLOW_PAGE_LOCKS = ON) ON [PRIMARY]
) ON [PRIMARY] TEXTIMAGE_ON [PRIMARY]


GO
SET ANSI_PADDING OFF
GO
/****** Object:  Table [dbo].[users]    Script Date: 9/17/2014 6:20:03 AM ******/
SET ANSI_NULLS ON
GO
SET QUOTED_IDENTIFIER ON
GO
SET ANSI_PADDING ON
GO
CREATE TABLE [dbo].[users](
	[username] [varchar](250) NOT NULL,
	[password] [varchar](max) NOT NULL,
	[pass_details] [varchar](max) NULL,
	[created_at] [datetime] NOT NULL,
 CONSTRAINT [PK_users_username] PRIMARY KEY CLUSTERED
(
	[username] ASC
)WITH (PAD_INDEX = OFF, STATISTICS_NORECOMPUTE = OFF, IGNORE_DUP_KEY = OFF, ALLOW_ROW_LOCKS = ON, ALLOW_PAGE_LOCKS = ON) ON [PRIMARY]
) ON [PRIMARY] TEXTIMAGE_ON [PRIMARY]

GO
SET ANSI_PADDING OFF
GO
/****** Object:  Table [dbo].[vcard]    Script Date: 9/17/2014 6:20:03 AM ******/
SET ANSI_NULLS ON
GO
SET QUOTED_IDENTIFIER ON
GO
SET ANSI_PADDING ON
GO
CREATE TABLE [dbo].[vcard](
	[username] [varchar](150) NOT NULL,
	[server] [varchar](150) NOT NULL,
	[vcard] [varchar](max) NOT NULL,
	[created_at] [datetime] NOT NULL,
 CONSTRAINT [PK_vcard_username] PRIMARY KEY CLUSTERED
(
	[username] ASC,
	[server] ASC
)WITH (PAD_INDEX = OFF, STATISTICS_NORECOMPUTE = OFF, IGNORE_DUP_KEY = OFF, ALLOW_ROW_LOCKS = ON, ALLOW_PAGE_LOCKS = ON) ON [PRIMARY]
) ON [PRIMARY] TEXTIMAGE_ON [PRIMARY]

GO
SET ANSI_PADDING OFF
GO
/****** Object:  Table [dbo].[vcard_search]    Script Date: 9/17/2014 6:20:03 AM ******/
SET ANSI_NULLS ON
GO
SET QUOTED_IDENTIFIER ON
GO
SET ANSI_PADDING ON
GO
CREATE TABLE [dbo].[vcard_search](
	[username] [varchar](150) NOT NULL,
	[lusername] [varchar](100) NOT NULL,
	[server] [varchar](150) NOT NULL,
	[fn] [varchar](max) NOT NULL,
	[lfn] [varchar](250) NOT NULL,
	[family] [varchar](max) NOT NULL,
	[lfamily] [varchar](250) NOT NULL,
	[given] [varchar](max) NOT NULL,
	[lgiven] [varchar](250) NOT NULL,
	[middle] [varchar](max) NOT NULL,
	[lmiddle] [varchar](250) NOT NULL,
	[nickname] [varchar](max) NOT NULL,
	[lnickname] [varchar](250) NOT NULL,
	[bday] [varchar](max) NOT NULL,
	[lbday] [varchar](250) NOT NULL,
	[ctry] [varchar](max) NOT NULL,
	[lctry] [varchar](250) NOT NULL,
	[locality] [varchar](max) NOT NULL,
	[llocality] [varchar](250) NOT NULL,
	[email] [varchar](max) NOT NULL,
	[lemail] [varchar](250) NOT NULL,
	[orgname] [varchar](max) NOT NULL,
	[lorgname] [varchar](250) NOT NULL,
	[orgunit] [varchar](max) NOT NULL,
	[lorgunit] [varchar](250) NOT NULL,
 CONSTRAINT [PK_vcard_search_lusername] PRIMARY KEY CLUSTERED
(
	[lusername] ASC,
	[server] ASC
)WITH (PAD_INDEX = OFF, STATISTICS_NORECOMPUTE = OFF, IGNORE_DUP_KEY = OFF, ALLOW_ROW_LOCKS = ON, ALLOW_PAGE_LOCKS = ON) ON [PRIMARY]
) ON [PRIMARY] TEXTIMAGE_ON [PRIMARY]

GO
SET ANSI_PADDING OFF
GO
ALTER TABLE [dbo].[offline_message] ADD  DEFAULT (NULL) FOR [expire]
GO
ALTER TABLE [dbo].[privacy_list] ADD  DEFAULT (getdate()) FOR [created_at]
GO
ALTER TABLE [dbo].[privacy_list_data] ADD  DEFAULT (NULL) FOR [id]
GO
ALTER TABLE [dbo].[private_storage] ADD  DEFAULT (getdate()) FOR [created_at]
GO
ALTER TABLE [dbo].[rosterusers] ADD  DEFAULT (getdate()) FOR [created_at]
GO
ALTER TABLE [dbo].[spool] ADD  DEFAULT (getdate()) FOR [created_at]
GO
ALTER TABLE [dbo].[users] ADD  DEFAULT (getdate()) FOR [created_at]
GO
ALTER TABLE [dbo].[vcard] ADD  DEFAULT (N'') FOR [username]
GO
ALTER TABLE [dbo].[vcard] ADD  DEFAULT (N'') FOR [server]
GO
ALTER TABLE [dbo].[vcard] ADD  DEFAULT (getdate()) FOR [created_at]
GO
ALTER TABLE [dbo].[vcard_search] ADD  DEFAULT (N'') FOR [lusername]
GO
ALTER TABLE [dbo].[vcard_search] ADD  DEFAULT (N'') FOR [server]
GO
EXEC sys.sp_addextendedproperty @name=N'MS_SSMA_SOURCE', @value=N'ejabberd.last' , @level0type=N'SCHEMA',@level0name=N'dbo', @level1type=N'TABLE',@level1name=N'last'
GO
EXEC sys.sp_addextendedproperty @name=N'MS_SSMA_SOURCE', @value=N'ejabberd.mam_config' , @level0type=N'SCHEMA',@level0name=N'dbo', @level1type=N'TABLE',@level1name=N'mam_config'
GO
EXEC sys.sp_addextendedproperty @name=N'MS_SSMA_SOURCE', @value=N'ejabberd.mam_message' , @level0type=N'SCHEMA',@level0name=N'dbo', @level1type=N'TABLE',@level1name=N'mam_message'
GO
EXEC sys.sp_addextendedproperty @name=N'MS_SSMA_SOURCE', @value=N'ejabberd.mam_muc_message' , @level0type=N'SCHEMA',@level0name=N'dbo', @level1type=N'TABLE',@level1name=N'mam_muc_message'
GO
EXEC sys.sp_addextendedproperty @name=N'MS_SSMA_SOURCE', @value=N'ejabberd.mam_server_user' , @level0type=N'SCHEMA',@level0name=N'dbo', @level1type=N'TABLE',@level1name=N'mam_server_user'
GO
EXEC sys.sp_addextendedproperty @name=N'MS_SSMA_SOURCE', @value=N'ejabberd.mam_user' , @level0type=N'SCHEMA',@level0name=N'dbo', @level1type=N'TABLE',@level1name=N'mam_user'
GO
EXEC sys.sp_addextendedproperty @name=N'MS_SSMA_SOURCE', @value=N'ejabberd.offline_message' , @level0type=N'SCHEMA',@level0name=N'dbo', @level1type=N'TABLE',@level1name=N'offline_message'
GO
EXEC sys.sp_addextendedproperty @name=N'MS_SSMA_SOURCE', @value=N'ejabberd.privacy_default_list' , @level0type=N'SCHEMA',@level0name=N'dbo', @level1type=N'TABLE',@level1name=N'privacy_default_list'
GO
EXEC sys.sp_addextendedproperty @name=N'MS_SSMA_SOURCE', @value=N'ejabberd.privacy_list' , @level0type=N'SCHEMA',@level0name=N'dbo', @level1type=N'TABLE',@level1name=N'privacy_list'
GO
EXEC sys.sp_addextendedproperty @name=N'MS_SSMA_SOURCE', @value=N'ejabberd.privacy_list_data' , @level0type=N'SCHEMA',@level0name=N'dbo', @level1type=N'TABLE',@level1name=N'privacy_list_data'
GO
EXEC sys.sp_addextendedproperty @name=N'MS_SSMA_SOURCE', @value=N'ejabberd.private_storage' , @level0type=N'SCHEMA',@level0name=N'dbo', @level1type=N'TABLE',@level1name=N'private_storage'
GO
EXEC sys.sp_addextendedproperty @name=N'MS_SSMA_SOURCE', @value=N'ejabberd.roster_version' , @level0type=N'SCHEMA',@level0name=N'dbo', @level1type=N'TABLE',@level1name=N'roster_version'
GO
EXEC sys.sp_addextendedproperty @name=N'MS_SSMA_SOURCE', @value=N'ejabberd.rostergroups' , @level0type=N'SCHEMA',@level0name=N'dbo', @level1type=N'TABLE',@level1name=N'rostergroups'
GO
EXEC sys.sp_addextendedproperty @name=N'MS_SSMA_SOURCE', @value=N'ejabberd.rosterusers' , @level0type=N'SCHEMA',@level0name=N'dbo', @level1type=N'TABLE',@level1name=N'rosterusers'
GO
EXEC sys.sp_addextendedproperty @name=N'MS_SSMA_SOURCE', @value=N'ejabberd.spool' , @level0type=N'SCHEMA',@level0name=N'dbo', @level1type=N'TABLE',@level1name=N'spool'
GO
EXEC sys.sp_addextendedproperty @name=N'MS_SSMA_SOURCE', @value=N'ejabberd.users' , @level0type=N'SCHEMA',@level0name=N'dbo', @level1type=N'TABLE',@level1name=N'users'
GO
EXEC sys.sp_addextendedproperty @name=N'MS_SSMA_SOURCE', @value=N'ejabberd.vcard' , @level0type=N'SCHEMA',@level0name=N'dbo', @level1type=N'TABLE',@level1name=N'vcard'
GO
EXEC sys.sp_addextendedproperty @name=N'MS_SSMA_SOURCE', @value=N'ejabberd.vcard_search' , @level0type=N'SCHEMA',@level0name=N'dbo', @level1type=N'TABLE',@level1name=N'vcard_search'
GO
