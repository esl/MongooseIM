GO
CREATE TABLE [dbo].[test_types](
    [unicode] [nvarchar](max),
    [binary_data_8k] [varbinary](8000),
    [binary_data_65k] [varbinary](max),
    [binary_data_16m] [varbinary](max), -- varbinary(max) is 2^31-1 bytes
    [ascii_char] char(1),
    [ascii_string] varchar(250), -- limited usage, base64-like stuff
    [int32] [int],
    [int64] [bigint],
    [int8] tinyint,
    [enum_char] [nvarchar](1),
    [bool_flag] smallint
)
GO

/****** Object:  Table [dbo].[last]    Script Date: 9/17/2014 6:20:03 AM ******/
SET ANSI_NULLS ON
GO
SET QUOTED_IDENTIFIER ON
GO
SET ANSI_PADDING ON
GO
CREATE TABLE [dbo].[last](
	[username] [nvarchar](250) NOT NULL,
	[seconds] [int] NOT NULL,
	[state] [nvarchar](max) NOT NULL,
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
	[remote_jid] [nvarchar](250) NOT NULL,
	[behaviour] [nvarchar](1) NOT NULL
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
	[from_jid] [nvarchar](250) NOT NULL,
	[remote_bare_jid] [nvarchar](250) NOT NULL,
	[remote_resource] [nvarchar](250) NOT NULL,
	[direction] [nvarchar](1) NOT NULL,
	[message] [varbinary](max) NOT NULL,
	[search_body] [nvarchar](max) NOT NULL,
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
/* id is not unique across the table, because we create copies of the same
 * archive for different users in our tests */
CREATE TABLE [dbo].[mam_muc_message](
	[id] [bigint] NOT NULL,
	[room_id] [bigint] NOT NULL,
	[sender_id] [bigint] NOT NULL,
	[nick_name] [nvarchar](250) NOT NULL,
	[message] [varbinary](max) NOT NULL,
	[search_body] [nvarchar](max) NOT NULL,
 CONSTRAINT [PK_mam_muc_message_id] PRIMARY KEY CLUSTERED
(
    [room_id] ASC,
	[id] ASC
)WITH (PAD_INDEX = OFF, STATISTICS_NORECOMPUTE = OFF, IGNORE_DUP_KEY = OFF, ALLOW_ROW_LOCKS = ON, ALLOW_PAGE_LOCKS = ON) ON [PRIMARY]
) ON [PRIMARY] TEXTIMAGE_ON [PRIMARY]
GO

CREATE INDEX i_mam_muc_message_sender_id ON mam_muc_message(sender_id);
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
	[server] [nvarchar](250) NOT NULL,
	[user_name] [nvarchar](250) NOT NULL,
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
	[server] [nvarchar](250) NOT NULL,
	[username] [nvarchar](250) NOT NULL,
	[from_jid] [nvarchar](250) NOT NULL,
	[packet] [nvarchar](max) NOT NULL,
	[permanent_fields] [varbinary](max),
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
	[username] [nvarchar](250) NOT NULL,
	[name] [nvarchar](250) NOT NULL,
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
	[username] [nvarchar](250) NOT NULL,
	[name] [nvarchar](250) NOT NULL,
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
	[value] [nvarchar](max) NOT NULL,
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
    /* be aware of 900 bytes index length limit. nvarchar uses two bytes per char */
	[username] [nvarchar](200) NOT NULL, -- 250 in mysql
	[namespace] [nvarchar](250) NOT NULL,
	[data] [nvarchar](max) NOT NULL,
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
	[username] [nvarchar](250) NOT NULL,
	[version] [nvarchar](max) NOT NULL,
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
	[username] [nvarchar](250) NOT NULL,
	[jid] [nvarchar](250) NOT NULL,
	[grp] [nvarchar](max) NOT NULL
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
	[username] [nvarchar](200) NOT NULL, -- 200 in mysql
	[jid] [nvarchar](250) NOT NULL,
	[nick] [nvarchar](max) NOT NULL,
	[subscription] [char](1) NOT NULL,
	[ask] [char](1) NOT NULL,
	[askmessage] [nvarchar](max) NOT NULL,
	[server] [char](1) NOT NULL,
	[subscribe] [nvarchar](max) NOT NULL,
	[type] [nvarchar](max) NULL,
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
	[username] [nvarchar](250) NOT NULL,
	[password] [nvarchar](max) NOT NULL,
	[pass_details] [nvarchar](max) NULL,
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
	[username] [nvarchar](150) NOT NULL,
	[server] [nvarchar](150) NOT NULL,
	[vcard] [nvarchar](max) NOT NULL,
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
	[username] [nvarchar](150) NOT NULL,
	[lusername] [nvarchar](100) NOT NULL,
	[server] [nvarchar](150) NOT NULL,
	[fn] [nvarchar](max) NOT NULL,
	[lfn] [nvarchar](250) NOT NULL,
	[family] [nvarchar](max) NOT NULL,
	[lfamily] [nvarchar](250) NOT NULL,
	[given] [nvarchar](max) NOT NULL,
	[lgiven] [nvarchar](250) NOT NULL,
	[middle] [nvarchar](max) NOT NULL,
	[lmiddle] [nvarchar](250) NOT NULL,
	[nickname] [nvarchar](max) NOT NULL,
	[lnickname] [nvarchar](250) NOT NULL,
	[bday] [nvarchar](max) NOT NULL,
	[lbday] [nvarchar](250) NOT NULL,
	[ctry] [nvarchar](max) NOT NULL,
	[lctry] [nvarchar](250) NOT NULL,
	[locality] [nvarchar](max) NOT NULL,
	[llocality] [nvarchar](250) NOT NULL,
	[email] [nvarchar](max) NOT NULL,
	[lemail] [nvarchar](250) NOT NULL,
	[orgname] [nvarchar](max) NOT NULL,
	[lorgname] [nvarchar](250) NOT NULL,
	[orgunit] [nvarchar](max) NOT NULL,
	[lorgunit] [nvarchar](250) NOT NULL,
 CONSTRAINT [PK_vcard_search_lusername] PRIMARY KEY CLUSTERED
(
	[lusername] ASC,
	[server] ASC
)WITH (PAD_INDEX = OFF, STATISTICS_NORECOMPUTE = OFF, IGNORE_DUP_KEY = OFF, ALLOW_ROW_LOCKS = ON, ALLOW_PAGE_LOCKS = ON) ON [PRIMARY]
) ON [PRIMARY] TEXTIMAGE_ON [PRIMARY]

GO

CREATE TABLE [dbo].[muc_light_rooms](
    [id] [bigint] IDENTITY(1,1) NOT NULL UNIQUE,

    [luser] [NVARCHAR](200)  NOT NULL, -- 250 in mysql
    [lserver] NVARCHAR(250)  NOT NULL,
    [version] NVARCHAR(20)   NOT NULL,
    CONSTRAINT [PK_muc_light_rooms] PRIMARY KEY CLUSTERED(
        [lserver] ASC,
        [luser] ASC
    ))
GO

CREATE TABLE [dbo].[muc_light_occupants](
    [room_id] [bigint]        NOT NULL,
    [luser]   [NVARCHAR](200)  NOT NULL, -- 250 in mysql
    [lserver] [NVARCHAR](200)  NOT NULL, -- 250 in mysql
    [aff] TINYINT             NOT NULL,
    CONSTRAINT [PK_muc_light_occupants] PRIMARY KEY (
        [room_id] ASC,
        [lserver] ASC,
        [luser] ASC
    ))
GO

ALTER TABLE [dbo].[muc_light_occupants]
    ADD CONSTRAINT FK_occupants_muc_light_rooms
        FOREIGN KEY (room_id)
        REFERENCES [dbo].[muc_light_rooms](id)
        ON DELETE CASCADE
        ON UPDATE CASCADE
GO

CREATE INDEX i_muc_light_occupants_id ON muc_light_occupants(room_id)
GO
CREATE INDEX i_muc_light_occupants_us ON muc_light_occupants(lserver, luser)
GO

CREATE TABLE [dbo].[muc_light_config](
    room_id bigint          NOT NULL,
    opt NVARCHAR(100)        NOT NULL,
    val NVARCHAR(250)        NOT NULL,
    CONSTRAINT [PK_muc_light_config] PRIMARY KEY CLUSTERED(
        [room_id] ASC,
        [opt] ASC
    ))
GO

ALTER TABLE [dbo].[muc_light_config]
    ADD CONSTRAINT FK_config_muc_light_rooms
        FOREIGN KEY (room_id)
        REFERENCES [dbo].[muc_light_rooms](id)
        ON DELETE CASCADE
        ON UPDATE CASCADE
GO

CREATE TABLE dbo.muc_light_blocking(
    luser NVARCHAR(250)      NOT NULL,
    lserver NVARCHAR(250)    NOT NULL,
    what TINYINT            NOT NULL,
    who NVARCHAR(500)        NOT NULL
)
GO

CREATE INDEX i_muc_light_blocking ON muc_light_blocking(luser, lserver);
GO

-- luser, lserver and remote_bare_jid have 250 characters in MySQL
-- but here we are limited by index size (900 bytes)
CREATE TABLE dbo.inbox(
    luser NVARCHAR(150) NOT NULL,
    lserver NVARCHAR(150) NOT NULL,
    remote_bare_jid NVARCHAR(150) NOT NULL,
    content VARBINARY(max) NOT NULL,
    unread_count INT NOT NULL,
    msg_id NVARCHAR(250) NOT NULL,
    timestamp BIGINT NOT NULL,
    CONSTRAINT PK_inbox PRIMARY KEY CLUSTERED(
        luser ASC,
        lserver ASC,
        remote_bare_jid ASC
    )
)
GO

CREATE INDEX i_inbox_ts ON inbox(luser, lserver, timestamp);
GO

CREATE TABLE dbo.pubsub_nodes (
    nidx BIGINT           IDENTITY(1,1) PRIMARY KEY,
    p_key NVARCHAR(150)   NOT NULL,
    name NVARCHAR(150)    NOT NULL,
    type NVARCHAR(250)    NOT NULL,
    owners NVARCHAR(max)  NOT NULL,
    options NVARCHAR(max) NOT NULL
)
GO

CREATE UNIQUE INDEX i_pubsub_nodes_key_name ON pubsub_nodes(p_key, name);
GO

CREATE TABLE dbo.pubsub_node_collections (
    name NVARCHAR(150) NOT NULL,
    parent_name NVARCHAR(150) NOT NULL,
    CONSTRAINT PK_pubsub_node_collections PRIMARY KEY CLUSTERED(
        name ASC,
        parent_name ASC
    )
)
GO

CREATE TABLE dbo.pubsub_affiliations (
    nidx BIGINT NOT NULL,
    luser NVARCHAR(150) NOT NULL,
    lserver NVARCHAR(150) NOT NULL,
    aff TINYINT NOT NULL,
    CONSTRAINT PK_pubsub_affiliations PRIMARY KEY CLUSTERED(
        luser ASC,
        lserver ASC,
        nidx ASC
    )
)
GO

CREATE INDEX i_pubsub_affiliations_nidx ON pubsub_affiliations(nidx);
GO

CREATE TABLE dbo.pubsub_items (
    nidx BIGINT                      NOT NULL,
    itemid NVARCHAR(250)             NOT NULL,
    created_luser NVARCHAR(150)      NOT NULL,
    created_lserver NVARCHAR(150)    NOT NULL,
    created_at BIGINT                NOT NULL,
    modified_luser NVARCHAR(150)     NOT NULL,
    modified_lserver NVARCHAR(150)   NOT NULL,
    modified_lresource NVARCHAR(150) NOT NULL,
    modified_at BIGINT               NOT NULL,
    publisher NVARCHAR(max),
    payload VARBINARY(max)           NOT NULL,
    CONSTRAINT PK_pubsub_items PRIMARY KEY CLUSTERED(
	nidx ASC,
	itemid ASC
    )
)
GO

CREATE TABLE dbo.pubsub_last_item (
    nidx BIGINT                      NOT NULL PRIMARY KEY,
    itemid NVARCHAR(250)             NOT NULL,
    created_luser NVARCHAR(250)      NOT NULL,
    created_lserver NVARCHAR(250)    NOT NULL,
    created_at BIGINT                NOT NULL,
    payload VARBINARY(max)           NOT NULL
)
GO

-- we skip luser and lserver in this one as this is little chance (even impossible?)
-- to have itemid duplication for distinct users
CREATE INDEX i_pubsub_items_lus_nidx ON pubsub_items(created_luser, created_lserver, nidx);
GO
CREATE INDEX i_pubsub_items_nidx ON pubsub_items(nidx);
GO


CREATE TABLE dbo.pubsub_subscriptions (
    nidx BIGINT NOT NULL,
    luser NVARCHAR(150) NOT NULL,
    lserver NVARCHAR(150) NOT NULL,
    lresource NVARCHAR(150) NOT NULL,
    type TINYINT NOT NULL,
    sub_id NVARCHAR(125) NOT NULL,
    options NVARCHAR(max) NOT NULL
)
GO

CREATE INDEX i_pubsub_subscriptions_lus_nidx ON pubsub_subscriptions(luser, lserver, nidx);
GO
CREATE INDEX i_pubsub_subscriptions_nidx ON pubsub_subscriptions(nidx);
GO

CREATE TABLE event_pusher_push_subscription (
     owner_jid NVARCHAR(250),
     node NVARCHAR(250),
     pubsub_jid NVARCHAR(150),
     form NVARCHAR(max) NOT NULL,
     created_at BIGINT NOT NULL,
     CONSTRAINT PK_even_pusher_push_subscription PRIMARY KEY CLUSTERED(
	owner_jid ASC,
	node ASC,
	pubsub_jid ASC
    )
 )
GO

CREATE INDEX i_event_pusher_push_subscription ON event_pusher_push_subscription(owner_jid);
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

CREATE TABLE mongoose_cluster_id (
    k varchar(50) NOT NULL PRIMARY KEY,
    v text
);
