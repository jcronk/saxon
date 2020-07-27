<?xml version="1.0" encoding="UTF-8"?>
<xsl:stylesheet
    xmlns:xsl="http://www.w3.org/1999/XSL/Transform"
    xmlns:xs="http://www.w3.org/2001/XMLSchema" exclude-result-prefixes="#all" version="3.0">
    <xsl:mode on-no-match="shallow-copy"/>
    <xsl:template match="Test" expand-text="yes">
        <xsl:copy>{replace(., 'a test', 'a successful test')}</xsl:copy>
    </xsl:template>
</xsl:stylesheet>
