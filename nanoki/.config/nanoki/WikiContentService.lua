--------------------------------------------------------------------------------
-- Title:               WikiContentService.lua
-- Description:         Like a square peg in a round hole
-- Author:              Raphaël Szwarc http://alt.textdrive.com/lua/
-- Creation Date:       January 30, 2007
-- Legal:               Copyright (C) 2007 Raphaël Szwarc
--                      Under the terms of the MIT License
--                      http://www.opensource.org/licenses/mit-license.html
--------------------------------------------------------------------------------

-- import dependencies
local HTTP = require( 'HTTP' )
local Template = require( 'Template' )
local URL = require( 'URL' )
local URLPath = require( 'URLPath' )
local WikiContent = require( 'WikiContent' )
local WikiService = require( 'WikiService' )

local BaseLink = WikiService.BaseLink
local DateLink = WikiService.DateLink
local FeedLink = WikiService.FeedLink
local IndexLink = WikiService.IndexLink

local ContentIterator = WikiService.ContentIterator
local NameIterator = WikiService.NameIterator
local Today = WikiService.Today
local Yesterday = WikiService.Yesterday
local ThisWeek = WikiService.ThisWeek

local Encode = WikiService.Encode
local FormatDate = WikiService.FormatDate
local FormatDateTime = WikiService.FormatDateTime
local GetType = WikiService.GetType
local HTML = WikiService.HTML
local Path = WikiService.Path
local Tag = WikiService.Tag

local os = require( 'os' )
local table = require( 'table' )

local getmetatable = getmetatable
local require = require
local setmetatable = setmetatable
local tonumber = tonumber
local tostring = tostring

--------------------------------------------------------------------------------
-- WikiContentService
--------------------------------------------------------------------------------

module( 'WikiContentService' )
_VERSION = '1.0'

local self = setmetatable( _M, {} )
local meta = getmetatable( self )

--------------------------------------------------------------------------------
-- Utilities
--------------------------------------------------------------------------------

local function Message( self )
    local aModification = self.content.modification or 0
    local aTime = os.time()
    local anInterval = aTime - aModification
    
    if anInterval < 30 then
        local WikiMessage = require( 'WikiMessage' )
        local aText = FormatDateTime( self.content.modification )
        local aMessage = WikiMessage( ( 'Updated on %s.' ):format( aText ) )
        
        return aMessage
    end
end

local function EditorLink( self ) 
    local aContent = self.content
    local aTitle = Encode( aContent.data.title )
    
    if aContent.canWrite then
        local aLink = Encode( self.path( 'editor' ) )
    
        return ( '<a href=\'%s\' title=\'Editor\' rel=\'nofollow\'>%s</a>' ):format( aLink, aTitle )
    
    end
    
    return aTitle
end

local function Robot( aModification )
    local aTime = os.time()
    local anInterval = aTime - ( aModification or aTime )
    
    if anInterval > 86400 then
        return 'index,follow'
    end
    
    return 'noindex,nofollow'
end

local function Referer( self )
    if self.content.name ~= 'main' 
    and self.content.name ~= 'markdown-syntax-reference' 
    and HTTP.request.header[ 'referer' ] then
        local URL = require( 'URL' )
        local aURL = URL( HTTP.request.header[ 'referer' ] )
        local aReferer = self.toObject( self, aURL )
        
        if aReferer 
        and aReferer.content.name ~= 'date' 
        and aReferer.content.name ~= 'index' 
        and aReferer.content.name ~= 'main' 
        and aReferer.content.name ~= 'markdown-syntax-reference' 
        and aReferer.content.name ~= 'recent' 
        and aReferer.content.name ~= 'search' 
        and aReferer.content.exists then
            self.content.link = aReferer.content.name
        end
    end
end

--------------------------------------------------------------------------------
-- DAV Utitities
--------------------------------------------------------------------------------

local function DAVIterator( aContent )
    local aName = ( '%s.txt' ):format( aContent.data.title )
    local aCreation = aContent.creation
    local aModification = aContent.modification
    local aSize = aContent.text:len()
    local aResource = { name = aName, mode = 'file', creation = aCreation, modification = aModification, size = aSize }
    local aList = { aResource }
    local aCount = #aList
    local anIndex = 1
    
    aResource = { name = 'file', mode = 'directory', modification = aModification, size = 0 }
    aList[ #aList + 1 ] = aResource

    aCount = #aList
    
    return function()
        if anIndex <= aCount then
            local aResource = aList[ anIndex ]
            
            anIndex = anIndex + 1
            
            return aResource
        end
    end
end

local function DAVResource( aContent )
    local anIterator = DAVIterator( aContent )
    local aCreation = aContent.creation
    local aModification = aContent.modification
    local aResource = { iterator = anIterator, mode = 'directory', creation = aCreation, modification = aModification, size = 0 }
    
    return aResource
end

--------------------------------------------------------------------------------
-- Default content
--------------------------------------------------------------------------------

local function Log()
    local aContent = WikiContent( 'Log' )
    
    if not aContent.exists then
        local WikiDate = require( 'WikiDate' )
        local WikiFinder = require( 'WikiFinder' )
        local WikiRecent = require( 'WikiRecent' )
        local WikiSearch = require( 'WikiSearch' )
        
        aContent.by = 'file://nanoki:127.0.0.1@localhost/'
        aContent.title = 'Log'
        aContent.text = '   '
        aContent()
        
        WikiDate[ aContent.name ] = aContent.creation
        WikiFinder[ aContent.name ] = true
        WikiRecent[ aContent.name ] = aContent.modification
        WikiSearch[ aContent.name ] = aContent.name
    end
end

local function Nanoki()
    local aContent = WikiContent( 'nanoki' )
    
    if not aContent.exists then
        local File = require( 'File' )
        local WikiDate = require( 'WikiDate' )
        local WikiFinder = require( 'WikiFinder' )
        local WikiRecent = require( 'WikiRecent' )
        local WikiSearch = require( 'WikiSearch' )
        local aPath = ( '%sNanoki' ):format( require( 'Bundle' )() )
        local aDirectory = File( aPath )
        local aTemplate = Template[ 'Nanoki.txt' ]
        
        aContent.by = 'file://nanoki:127.0.0.1@localhost/'
        aContent.title = 'Nanoki'
        aContent.text = tostring( aTemplate )
        aContent()
        
        for aFile in aDirectory() do
            aContent.file = aFile
        end
        
        aContent.canWrite = false
        WikiDate[ aContent.name ] = aContent.creation
        WikiFinder[ aContent.name ] = true
        WikiRecent[ aContent.name ] = aContent.modification
        WikiSearch[ aContent.name ] = aContent.name
    end
end

local function Syntax()
    local aContent = WikiContent( 'markdown-syntax-reference' )
    
    if not aContent.exists then
        local WikiDate = require( 'WikiDate' )
        local WikiFinder = require( 'WikiFinder' )
        local WikiRecent = require( 'WikiRecent' )
        local WikiSearch = require( 'WikiSearch' )
        local aTemplate = Template[ 'MarkdownSyntaxReference.txt' ]
        
        aContent.by = 'file://nanoki:127.0.0.1@localhost/'
        aContent.title = 'Markdown syntax reference'
        aContent.text = tostring( aTemplate )
        aContent()
        
        WikiDate[ aContent.name ] = aContent.creation
        WikiFinder[ aContent.name ] = true
        WikiRecent[ aContent.name ] = aContent.modification
        WikiSearch[ aContent.name ] = aContent.name
    end
end

--------------------------------------------------------------------------------
-- Service methods
--------------------------------------------------------------------------------

self.toURL = function( aService, anObject )
    if anObject.content then
        local aPath = URLPath()
        
        aPath[ #aPath + 1 ] = anObject.content.name
        
        return URL( aService.prefix .. aPath )
    end
end

self.toObject = function( aService, aURL )
    local aPath = aURL.path
    local aName = aPath[ 1 ] or 'main'
    local aContent = WikiContent( aName )
    
    if aContent then
        local WikiContentService = require( 'WikiContentService' )
        local aService = WikiContentService( aContent )    
    
        return aService
    end
end

function self:getHtml()
    if self.content and self.content.exists then
        Referer( self )
    
        local aLayoutTemplate = Template[ 'WikiLayout.txt' ]
        local aTemplate = Template[ 'WikiContentService.txt' ]
        local aLinkTemplate = aTemplate[ 'link' ]
        local aLinkIterator, aLinkCount = ContentIterator( self.content.link )
        local aDate = os.date( '!*t', self.content.creation )
        local aDateLink = DateLink( aDate.year, aDate.month, aDate.day )
        
        aTemplate[ 'editorLink' ] = EditorLink( self )
        aTemplate[ 'version' ] = Encode(self.content.version )
        aTemplate[ 'title' ] = Encode( self.content.data.title )
        aTemplate[ 'dateLink' ] = Encode( aDateLink )
        aTemplate[ 'creation' ] = Encode( FormatDate( self.content.creation ) )
        aTemplate[ 'modification' ] = Encode( FormatDateTime( self.content.modification ) )
        aTemplate[ 'tag' ] = Tag( self.content.modification )
        aTemplate[ 'by' ] = WikiService.By( self.content, true )
        aTemplate[ 'message' ] = Message( self )
        aTemplate[ 'content' ] = HTML( self.content )
        
        for aContent, aURL in aLinkIterator do
            local aNameTemplate = aLinkTemplate[ 'names' ]
    
            aNameTemplate[ 'href' ] = Encode( aURL.path )
            aNameTemplate[ 'name' ] = Encode( aContent.title )
            
            aLinkTemplate[ 'names' ] = aNameTemplate
        end
        
        if aLinkCount == 0 then
            aLinkTemplate = nil
        end
        
        aTemplate[ 'link' ] = aLinkTemplate
        
        aLayoutTemplate[ 'baseLink' ] = Encode( BaseLink() )
        aLayoutTemplate[ 'indexLink' ] = Encode( IndexLink( self.content.prefix ) )
        aLayoutTemplate[ 'dateLink' ] = Encode( aDateLink )
        aLayoutTemplate[ 'feedLink' ] = FeedLink( self, 1 )
        aLayoutTemplate[ 'path' ] = Path( self )
        aLayoutTemplate[ 'query' ] = nil
        aLayoutTemplate[ 'robot' ] = Robot( self.content.modification )
        aLayoutTemplate[ 'title' ] = Encode( self.content.title )
        aLayoutTemplate[ 'content' ] = aTemplate
                
        return tostring( aLayoutTemplate )
    end
    
    return nil, self.path( 'editor' )
end

function self:getLua()
    if self.content and self.content.exists then
        local Data = require( 'Data' )
    
        HTTP.response.header[ 'content-type' ] = 'text/plain; charset=utf-8'
    
        return Data( self.content.data )
    end
    
    return nil, self.path( 'editor' )
end

function self:getTxt()
    if self.content and self.content.exists then
        HTTP.response.header[ 'content-type' ] = 'text/plain; charset=utf-8'
    
        return self.content.text
    end
    
    return nil, self.path( 'editor' )
end

function self:getXml()
    if self.content and self.content.exists then
        local WikiFeed = require( 'WikiFeed' )
        local anIterator = NameIterator( { self.content.name } )
        local aGenerator = HTML
        local aContext = { title = self.content.data.title, link = HTTP.request.url, creation = self.content.creation }
                
        HTTP.response.header[ 'content-type' ] = 'application/atom+xml; charset=utf-8'
    
        return tostring( WikiFeed( anIterator, aGenerator, aContext ) )
    end

    return nil, self.path( 'editor' )
end

function self:get( aType )
    if self.content and self.content.exists then
        return GetType( self, aType )
    end
    
    return nil, self.path( 'editor' )
end

function self:getEditor( aVersion )
    local WikiEditorService = require( 'WikiEditorService' )
    local aVersion = tonumber( aVersion )
    local aContent = WikiContent( self.content.name, aVersion )
    
    if aContent.canWrite then
        if not aContent.exists then
            aContent = self.content
        end
        
        if not aContent.exists then
            HTTP.response.status.code = 404
            HTTP.response.status.description = 'Not Found'
        end
        
        return WikiEditorService( aContent, self )
    end
end

function self:getFile()
    local WikiContentFileService = require( 'WikiContentFileService' )
    local aService = WikiContentFileService( self.content )
    
    return aService
end

--------------------------------------------------------------------------------
-- DAV service methods
--------------------------------------------------------------------------------

function self:options()
    HTTP.response.header[ 'allow' ] = 'DELETE, GET, HEAD, LOCK, MOVE, OPTIONS, PROPFIND, PUT, UNLOCK'
    HTTP.response.header[ 'content-type' ] = 'text/plain'
    
    return HTTP.response.header[ 'allow' ]
end

function self:propfind()
    local aContent = self.content
    
    if aContent and aContent.exists then
        local WikiDAV = require( 'WikiDAV' )
        local aResource = DAVResource( aContent )

        return WikiDAV( aResource ):propfind()
    end
end

--------------------------------------------------------------------------------
-- Metamethods
--------------------------------------------------------------------------------

function meta:__call( aContent )
    local aService = { content = aContent }
    
    Log()
    Nanoki()
    Syntax()
    
    setmetatable( aService, self )
    
    return aService
end

function meta:__concat( aValue )
    return tostring( self ) .. tostring( aValue )
end

function meta:__tostring()
    return ( '%s/%s' ):format( self._NAME, self._VERSION )
end

function self:__index( aKey )
    local aValue = getmetatable( self )[ aKey ]
    
    if not aValue and aKey:find( '^get.+' ) then
        local WikiContentFileService = require( 'WikiContentFileService' )
        local aName = aKey:match( '^get(.+)$' )
        local aService = WikiContentFileService( self.content, aName )

        aService.name = aName
        
        return function( self, anExtension ) 
            aService.name = ( '%s.%s' ):format( aName, anExtension or '' )
        
            return aService
        end
    end
    
    self[ aKey ] = aValue
    
    return aValue
end

function self:__concat( aValue )
    return tostring( self ) .. tostring( aValue )
end

function self:__tostring()
    return tostring( self.content.data.title )
end
