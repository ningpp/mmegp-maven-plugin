/*
 *    Copyright 2021-2022 the original author or authors.
 *
 *    Licensed under the Apache License, Version 2.0 (the "License");
 *    you may not use this file except in compliance with the License.
 *    You may obtain a copy of the License at
 *
 *       http://www.apache.org/licenses/LICENSE-2.0
 *
 *    Unless required by applicable law or agreed to in writing, software
 *    distributed under the License is distributed on an "AS IS" BASIS,
 *    WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 *    See the License for the specific language governing permissions and
 *    limitations under the License.
 */
package me.ningpp.mmegp;

import java.io.File;
import java.io.IOException;
import java.util.ArrayList;
import java.util.Collections;
import java.util.List;
import java.util.Properties;
import java.util.UUID;

import javax.xml.XMLConstants;
import javax.xml.parsers.DocumentBuilderFactory;
import javax.xml.parsers.ParserConfigurationException;

import org.apache.commons.lang3.StringUtils;
import org.apache.commons.lang3.tuple.Triple;
import org.apache.maven.plugin.AbstractMojo;
import org.apache.maven.plugin.MojoExecutionException;
import org.apache.maven.plugin.MojoFailureException;
import org.apache.maven.plugins.annotations.Component;
import org.apache.maven.plugins.annotations.LifecyclePhase;
import org.apache.maven.plugins.annotations.Mojo;
import org.apache.maven.plugins.annotations.Parameter;
import org.apache.maven.plugins.annotations.ResolutionScope;
import org.apache.maven.project.MavenProject;
import org.apache.maven.project.MavenProjectHelper;
import org.mybatis.generator.api.Plugin;
import org.mybatis.generator.config.CommentGeneratorConfiguration;
import org.mybatis.generator.config.Context;
import org.mybatis.generator.config.JavaClientGeneratorConfiguration;
import org.mybatis.generator.config.JavaModelGeneratorConfiguration;
import org.mybatis.generator.config.ModelType;
import org.mybatis.generator.config.PropertyRegistry;
import org.mybatis.generator.config.SqlMapGeneratorConfiguration;
import org.mybatis.generator.internal.ObjectFactory;
import org.sonatype.plexus.build.incremental.BuildContext;
import org.w3c.dom.Node;
import org.w3c.dom.NodeList;
import org.xml.sax.SAXException;

@Mojo(
        name = "generate",
        defaultPhase = LifecyclePhase.GENERATE_SOURCES,
        requiresDependencyResolution = ResolutionScope.COMPILE,
        requiresProject = true, 
        threadSafe = true
)
public class MmeCompileMojo extends AbstractMojo {

    private static final String XML_ID = "id";
    private static final String XML_NAME = "name";
    private static final String XML_VALUE = "value";
    private static final String XML_PROPERTY = "property";
    private static final String XML_TYPE = "type";
    private static final String XML_PLUGIN = "plugin";
    private static final String XML_CONTEXT = "context";
    private static final String XML_TARGET_PROJECT = "targetProject";

    /**
     * This is the directory into which the {@code .java} will be created.
     */
    @Parameter(
            required = false,
            property = "outputDirectory",
            defaultValue = "${project.build.directory}/generated-sources/mme/java"
    )
    private File outputDirectory;

    /**
     * This is the directory into which the {@code .xml} will be created.
     */
    @Parameter(required = false, property = "xmlOutputDirectory")
    private String xmlOutputDirectory;

    /**
     * This is the package name into which the Model Example Class will be created.
     */
    @Parameter( required = true, property = "modelPackageName" )
    private String modelPackageName;

    /**
     * This is the package name into which the Mapper Interface will be created.
     */
    @Parameter( required = true, property = "mapperPackageName" )
    private String mapperPackageName;

    /**
     * This is MBG javaClientGeneratorConfiguration.configurationType
     */
    @Parameter( required = false, property = "javaClientGeneratorConfigurationType" )
    private String javaClientGeneratorConfigurationType;

    /**
     * This is MBG CommentGeneratorConfiguration.configurationType
     */
    @Parameter( required = false, property = "commentGeneratorConfigurationType" )
    private String commentGeneratorConfigurationType;

    /**
     * This is the MyBatis Generator Context attribute 'TargetRuntime' 
     */
    @Parameter( required = false, property = "mbgContextTargetRuntime" )
    private String mbgContextTargetRuntime;

    /**
     * This is the MyBatis Generator Context attribute 'introspectedColumnImpl' 
     */
    @Parameter( required = false, property = "mbgContextIntrospectedColumnImpl" )
    private String mbgContextIntrospectedColumnImpl;

    /**
     * This is the generator config xml file path (like mbg config file).
     */
    @Parameter(required = false, property = "generatorConfigFilePath")
    private String generatorConfigFilePath;

    /**
     * This is the MetaInfoHandler class name config.
     */
    @Parameter(required = false, property = "metaInfoHandlerClassName")
    private String metaInfoHandlerClassName;

    @Parameter(defaultValue = "${project}", readonly = true)
    protected MavenProject project;

    @Component
    protected BuildContext buildContext;

    @Component
    protected MavenProjectHelper projectHelper;

    @Override
    public void execute() throws MojoExecutionException, MojoFailureException {
        String modelFileDir = project.getCompileSourceRoots().get(0) + File.separator + 
                modelPackageName.replace('.', File.separatorChar) + File.separator;
        String exampleFileDir = outputDirectory.getAbsolutePath() + File.separator + 
                modelPackageName.replace('.', File.separatorChar) + File.separator;
        String mapperFileDir = outputDirectory.getAbsolutePath() + File.separator + 
                mapperPackageName.replace('.', File.separatorChar) + File.separator;
        String xmlFileDir;
        if (StringUtils.isEmpty(xmlOutputDirectory)) {
            xmlFileDir = outputDirectory.getAbsolutePath() + File.separator + 
                    mapperPackageName.replace('.', File.separatorChar) + File.separator;
        } else {
            xmlFileDir = outputDirectory.getAbsolutePath() + File.separator + 
                    xmlOutputDirectory + File.separator;
        }

        MetaInfoHandler metaInfoHandler = createMetaInfoHandler(metaInfoHandlerClassName);

        try {
            List<Triple<String, Properties, List<PluginInfo>>> triples = parseContexts(generatorConfigFilePath);
            for (Triple<String, Properties, List<PluginInfo>> triple : triples) {
                Context context = new Context(ModelType.FLAT);
                context.setId(triple.getLeft());
                context.getProperties().putAll(triple.getMiddle());
                context.setTargetRuntime(mbgContextTargetRuntime);
                context.setIntrospectedColumnImpl(mbgContextIntrospectedColumnImpl);

                CommentGeneratorConfiguration commentGeneratorConfiguration = new CommentGeneratorConfiguration();
                commentGeneratorConfiguration.addProperty(PropertyRegistry.COMMENT_GENERATOR_SUPPRESS_DATE, Boolean.FALSE.toString());
                commentGeneratorConfiguration.addProperty(PropertyRegistry.COMMENT_GENERATOR_SUPPRESS_ALL_COMMENTS, Boolean.TRUE.toString());
                commentGeneratorConfiguration.addProperty(PropertyRegistry.COMMENT_GENERATOR_ADD_REMARK_COMMENTS, Boolean.FALSE.toString());
                commentGeneratorConfiguration.addProperty(PropertyRegistry.COMMENT_GENERATOR_DATE_FORMAT, "");
                if (StringUtils.isNotEmpty(commentGeneratorConfigurationType)) {
                    commentGeneratorConfiguration.setConfigurationType(commentGeneratorConfigurationType);
                }
                context.setCommentGeneratorConfiguration(commentGeneratorConfiguration);

                SqlMapGeneratorConfiguration sqlMapGeneratorConfiguration = new SqlMapGeneratorConfiguration();
                sqlMapGeneratorConfiguration.setTargetProject(outputDirectory.getAbsolutePath());
                if (StringUtils.isEmpty(xmlOutputDirectory)) {
                    sqlMapGeneratorConfiguration.setTargetPackage(mapperPackageName);
                } else {
                    sqlMapGeneratorConfiguration.setTargetPackage(xmlOutputDirectory);
                }
                context.setSqlMapGeneratorConfiguration(sqlMapGeneratorConfiguration);

                JavaClientGeneratorConfiguration javaClientGeneratorCfg = new JavaClientGeneratorConfiguration();
                javaClientGeneratorCfg.setTargetProject(outputDirectory.getAbsolutePath());
                javaClientGeneratorCfg.setConfigurationType(javaClientGeneratorConfigurationType);
                javaClientGeneratorCfg.setTargetPackage(mapperPackageName);
                context.setJavaClientGeneratorConfiguration(javaClientGeneratorCfg);

                JavaModelGeneratorConfiguration jmgConfig = new JavaModelGeneratorConfiguration();
                jmgConfig.setTargetProject(outputDirectory.getAbsolutePath());
                jmgConfig.setTargetPackage(modelPackageName);
                jmgConfig.addProperty(PropertyRegistry.MODEL_GENERATOR_EXAMPLE_PACKAGE, modelPackageName);
                jmgConfig.addProperty(PropertyRegistry.COMMENT_GENERATOR_SUPPRESS_ALL_COMMENTS, Boolean.TRUE.toString());
                context.setJavaModelGeneratorConfiguration(jmgConfig);

                //为了初始化pluginAggregator
                context.generateFiles(new NullProgressCallback(), Collections.emptyList(), 
                        Collections.emptyList(), Collections.emptyList(), Collections.emptyList(), Collections.emptyList());

                List<PluginInfo> pluginInfos = triple.getRight();
                List<Plugin> plugins = new ArrayList<>();
                Object[] initPluginArgs = null;
                for (PluginInfo pluginInfo : pluginInfos) {
                    Plugin plugin = (Plugin) pluginInfo.getPluginClass().getConstructors()[0].newInstance(initPluginArgs);
                    plugin.setContext(context);
                    plugin.setProperties(pluginInfo.getProperties());
                    plugins.add(plugin);
                }

                MmeCompileUtil.generate(context, modelPackageName, modelFileDir, exampleFileDir, 
                        mapperPackageName, mapperFileDir, xmlFileDir, 
                        metaInfoHandler, plugins);
            }
        } catch (Exception e) {
            throw new MojoExecutionException("Generate MyBatis Model Example File Error!", e);
        }

        projectHelper.addResource(project, outputDirectory.getAbsolutePath(), 
                List.of("**/*.xml"), Collections.emptyList());
        project.addCompileSourceRoot(outputDirectory.getAbsolutePath());
        buildContext.refresh(outputDirectory);
    }

    private MetaInfoHandler createMetaInfoHandler(String metaInfoHandlerClassName) {
        if (StringUtils.isBlank(metaInfoHandlerClassName)) {
            return null;
        }
        return (MetaInfoHandler) ObjectFactory.createInternalObject(metaInfoHandlerClassName.trim().strip());
    }

    private List<Triple<String, Properties, List<PluginInfo>>> parseContexts(String xmlFilePath) throws SAXException, IOException, ParserConfigurationException, 
        IllegalArgumentException, ReflectiveOperationException, SecurityException {
        if (StringUtils.isEmpty(xmlFilePath)) {
            return new ArrayList<>(0);
        }
        File xmlFile = new File(xmlFilePath);
        if (! xmlFile.exists()) {
            throw new IllegalArgumentException("plugin xml file does not exist! xmlFilePath is " + xmlFilePath);
        }

        DocumentBuilderFactory factory = DocumentBuilderFactory.newInstance();
        factory.setFeature(XMLConstants.FEATURE_SECURE_PROCESSING, true);
        NodeList nodeList = factory.newDocumentBuilder().parse(xmlFile).getDocumentElement().getChildNodes();
        int nodeLength = nodeList.getLength();
        List<Triple<String, Properties, List<PluginInfo>>> triples = new ArrayList<>();
        for (int i = 0; i < nodeLength; i++) {
            Node node = nodeList.item(i);
            if (! XML_CONTEXT.equals(node.getNodeName())) {
                continue;
            }

            String id = null;
            Node idNode = node.getAttributes().getNamedItem(XML_ID);
            if (idNode != null) {
                id = idNode.getTextContent();
            }
            if (StringUtils.isEmpty(id)) {
                id = UUID.randomUUID().toString();
            }

            Properties properties = new Properties(); 
            List<PluginInfo> pluginInfos = new ArrayList<>();
            NodeList contextChildNodes = node.getChildNodes();
            int contextChildNodeLength = contextChildNodes.getLength();
            for (int j = 0; j < contextChildNodeLength; j++) {
                Node childNode = contextChildNodes.item(j);
                if (XML_PLUGIN.equals(childNode.getNodeName())) {
                    pluginInfos.add(parsePlugin(childNode));
                } else if (XML_PROPERTY.equals(childNode.getNodeName())) {
                    properties.put(childNode.getAttributes().getNamedItem(XML_NAME).getTextContent(), 
                            childNode.getAttributes().getNamedItem(XML_VALUE).getTextContent());
                }
            }
            triples.add(Triple.of(id, properties, pluginInfos));
        }
        return triples;
    }

    private static class PluginInfo {
        private final Class<?> pluginClass;
        private final Properties properties;
        public PluginInfo(Class<?> pluginClass, Properties properties) {
            super();
            this.pluginClass = pluginClass;
            this.properties = properties;
        }
        public Class<?> getPluginClass() {
            return pluginClass;
        }
        public Properties getProperties() {
            return properties;
        }
    }

    private PluginInfo parsePlugin(Node pluginNode) throws IllegalArgumentException, ReflectiveOperationException, SecurityException {
        Class<?> pluginClass = Class.forName(pluginNode.getAttributes().getNamedItem(XML_TYPE).getTextContent());
        NodeList propertyNodes = pluginNode.getChildNodes();
        int propertyNodeLength = propertyNodes.getLength();
        Properties properties = new Properties();
        for (int i = 0; i < propertyNodeLength; i++) {
            Node propertyNode = propertyNodes.item(i);
            if (XML_PROPERTY.equals(propertyNode.getNodeName())) {
                String name = propertyNode.getAttributes().getNamedItem(XML_NAME).getTextContent();
                String prefix = "";
                if (XML_TARGET_PROJECT.equals(name)) {
                    prefix = project.getBasedir().getAbsolutePath();
                }
                properties.put(name, prefix + File.separator + propertyNode.getAttributes().getNamedItem(XML_VALUE).getTextContent());
            }
        }
        return new PluginInfo(pluginClass, properties);
    }
}
