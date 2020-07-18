<?xml version="1.0" encoding="UTF-8"?>
<xsl:stylesheet
    xmlns:xsl="http://www.w3.org/1999/XSL/Transform"
    xmlns:xs="http://www.w3.org/2001/XMLSchema" exclude-result-prefixes="#all" expand-text="true" version="3.0">
    <xsl:param name="test" select="false()" static="yes"/>
    <xsl:param name="string" static="yes"/>
    <xsl:mode on-no-match="shallow-copy"/>
    <xsl:output omit-xml-declaration="yes"/>
    <xsl:template match="/">
        <xsl:message>Starting the transformation!!</xsl:message>
        <xsl:apply-templates/>
    </xsl:template>
    <xsl:template match="Test">
        <xsl:message>
            Matched on "Test":
            <xsl:sequence select="."/>
        </xsl:message>
        <xsl:copy>This shows that the text has now changed!</xsl:copy>
        <xsl:copy use-when="$test">String contents were: {$string}</xsl:copy>
    </xsl:template>
</xsl:stylesheet>
