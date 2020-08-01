<?xml version="1.0" encoding="UTF-8"?>
<xsl:stylesheet
    xmlns:xsl="http://www.w3.org/1999/XSL/Transform"
    xmlns:xs="http://www.w3.org/2001/XMLSchema" exclude-result-prefixes="#all" version="3.0">
    <xsl:mode on-no-match="shallow-skip"/>
    <xsl:output omit-xml-declaration="yes"/>
    <xsl:template match="Test" as="xs:string" expand-text="yes">
        <xsl:sequence select="."/>
    </xsl:template>
</xsl:stylesheet>
