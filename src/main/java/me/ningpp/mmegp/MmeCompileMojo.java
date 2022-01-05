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

import javax.xml.XMLConstants;
import javax.xml.parsers.DocumentBuilderFactory;
import javax.xml.parsers.ParserConfigurationException;

import org.apache.commons.lang3.StringUtils;
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
import org.mybatis.generator.internal.NullProgressCallback;
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

        try {
            Context context = new Context(ModelType.FLAT);
            context.setTargetRuntime(mbgContextTargetRuntime);
            context.setIntrospectedColumnImpl(mbgContextIntrospectedColumnImpl);
            
            CommentGeneratorConfiguration commentGeneratorConfiguration = new CommentGeneratorConfiguration();
            commentGeneratorConfiguration.addProperty(PropertyRegistry.COMMENT_GENERATOR_SUPPRESS_DATE, Boolean.FALSE.toString());
            commentGeneratorConfiguration.addProperty(PropertyRegistry.COMMENT_GENERATOR_SUPPRESS_ALL_COMMENTS, Boolean.TRUE.toString());
            commentGeneratorConfiguration.addProperty(PropertyRegistry.COMMENT_GENERATOR_ADD_REMARK_COMMENTS, Boolean.FALSE.toString());
            commentGeneratorConfiguration.addProperty(PropertyRegistry.COMMENT_GENERATOR_DATE_FORMAT, "");
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
            context.generateFiles(new NullProgressCallback(), Collections.emptyList(), Collections.emptyList(), Collections.emptyList(), Collections.emptyList());

            MmeCompileUtil.generate(context, modelPackageName, modelFileDir, exampleFileDir, 
                    mapperPackageName, mapperFileDir, xmlFileDir, 
                    parsePlugins(context, generatorConfigFilePath));
        } catch (Exception e) {
            throw new MojoExecutionException("Generate MyBatis Model Example File Error!", e);
        }

        projectHelper.addResource(project, outputDirectory.getAbsolutePath(), 
                List.of("**/*.xml"), Collections.emptyList());
        project.addCompileSourceRoot(outputDirectory.getAbsolutePath());
        buildContext.refresh(outputDirectory);
    }

    private List<Plugin> parsePlugins(Context context, String xmlFilePath) throws SAXException, IOException, ParserConfigurationException, 
        IllegalArgumentException, ReflectiveOperationException, SecurityException {
        if (StringUtils.isEmpty(xmlFilePath)) {
            return new ArrayList<>(0);
        }
        File xmlFile = new File(xmlFilePath);
        if (! xmlFile.exists()) {
            throw new IllegalArgumentException("plugin xml file does not exist! xmlFilePath is " + xmlFilePath);
        }
        List<Plugin> plugins = new ArrayList<>();

        DocumentBuilderFactory factory = DocumentBuilderFactory.newInstance();
        factory.setFeature(XMLConstants.FEATURE_SECURE_PROCESSING, true);
        NodeList nodeList = factory.newDocumentBuilder().parse(xmlFile).getDocumentElement().getChildNodes();
        int nodeLength = nodeList.getLength();
        for (int i = 0; i < nodeLength; i++) {
            Node node = nodeList.item(i);
            if (! "context".equals(node.getNodeName())) {
                continue;
            }
            NodeList contextChildNodes = node.getChildNodes();
            int contextChildNodeLength = contextChildNodes.getLength();
            for (int j = 0; j < contextChildNodeLength; j++) {
                Node pluginNode = contextChildNodes.item(j);
                if ("plugin".equals(pluginNode.getNodeName())) {
                    plugins.add(createPlugin(context, pluginNode));
                }
            }
        }
        return plugins;
    }

    private Plugin createPlugin(Context context, Node pluginNode) throws IllegalArgumentException, ReflectiveOperationException, SecurityException {
        Object[] initargs = null;
        Class<?> pluginClass = Class.forName(pluginNode.getAttributes().getNamedItem("type").getTextContent());
        Plugin plugin = (Plugin) pluginClass.getConstructors()[0].newInstance(initargs);
        NodeList propertyNodes = pluginNode.getChildNodes();
        int propertyNodeLength = propertyNodes.getLength();
        Properties properties = new Properties();
        for (int i = 0; i < propertyNodeLength; i++) {
            Node propertyNode = propertyNodes.item(i);
            if ("property".equals(propertyNode.getNodeName())) {
                String name = propertyNode.getAttributes().getNamedItem("name").getTextContent();
                String prefix = "";
                if ("targetProject".equals(name)) {
                    prefix = project.getBasedir().getAbsolutePath();
                }
                properties.put(name, prefix + File.separator + propertyNode.getAttributes().getNamedItem("value").getTextContent());
            }
        }
        plugin.setContext(context);
        plugin.setProperties(properties);
        return plugin;
    }
}
