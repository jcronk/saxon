<?xml version="1.0" encoding="UTF-8"?>
<xsl:stylesheet
    xmlns:xsl="http://www.w3.org/1999/XSL/Transform"
    xmlns:xs="http://www.w3.org/2001/XMLSchema" exclude-result-prefixes="#all" version="3.0">
    <xsl:output method="text" omit-xml-declaration="yes"/>
    <xsl:param name="ss-param"/>
    <!-- This is the default name for an initial template, but I don't care what it's called, we just need a convention -->
    <xsl:template name="main" as="xs:string*">
        <xsl:param name="input" as="xs:string" select="'default'"/>
        <xsl:sequence expand-text="yes">Found template param "{$input}"</xsl:sequence>
        <xsl:sequence expand-text="yes">Stylesheet parameter was "{$ss-param}"</xsl:sequence>
    </xsl:template>
</xsl:stylesheet>
