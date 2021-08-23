/*
 *    Copyright 2021 the original author or authors.
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
import java.util.Arrays;
import java.util.Collections;
import java.util.Properties;

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
     * This is the root directory into which the {@code .xml} will be created.
     */
    @Parameter(
            required = false,
            property = "xmlRootDirectory",
            defaultValue = "${project.build.directory}/generated-sources/mme/resources"
    )
    private File xmlRootDirectory;

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

    @Parameter( required = false, property = "mapperExtGeneratorPluginClassName" )
    private String mapperExtGeneratorPluginClassName;

    @Parameter( required = false, property = "mapperExtXmlGeneratorPluginClassName" )
    private String mapperExtXmlGeneratorPluginClassName;

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
            xmlFileDir = xmlRootDirectory.getAbsolutePath() + File.separator + 
                    mapperPackageName.replace('.', File.separatorChar) + File.separator;
        } else {
            xmlFileDir = xmlRootDirectory.getAbsolutePath() + File.separator + 
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
            context.setSqlMapGeneratorConfiguration(sqlMapGeneratorConfiguration);

            JavaClientGeneratorConfiguration javaClientGeneratorCfg = new JavaClientGeneratorConfiguration();
            javaClientGeneratorCfg.setConfigurationType("XMLMAPPER");
            javaClientGeneratorCfg.setTargetPackage(mapperPackageName);
            context.setJavaClientGeneratorConfiguration(javaClientGeneratorCfg);

            JavaModelGeneratorConfiguration jmgConfig = new JavaModelGeneratorConfiguration();
            jmgConfig.setTargetPackage(modelPackageName);
            jmgConfig.addProperty(PropertyRegistry.MODEL_GENERATOR_EXAMPLE_PACKAGE, modelPackageName);
            jmgConfig.addProperty(PropertyRegistry.COMMENT_GENERATOR_SUPPRESS_ALL_COMMENTS, Boolean.TRUE.toString());
            context.setJavaModelGeneratorConfiguration(jmgConfig);

            //为了初始化pluginAggregator
            context.generateFiles(new NullProgressCallback(), Collections.emptyList(), Collections.emptyList(), Collections.emptyList(), Collections.emptyList());

            Object[] initargs = null;

            Plugin mapperExtGeneratorPlugin;
            if (StringUtils.isNotEmpty(mapperExtXmlGeneratorPluginClassName)) {
                mapperExtGeneratorPlugin = (Plugin) Class.forName(mapperExtGeneratorPluginClassName).getDeclaredConstructors()[0].newInstance(initargs);
                mapperExtGeneratorPlugin.setContext(context);
                mapperExtGeneratorPlugin.setProperties(new Properties());
            } else {
                mapperExtGeneratorPlugin = null;
            }

            Plugin mapperExtXmlGeneratorPlugin;
            if (StringUtils.isNotEmpty(mapperExtXmlGeneratorPluginClassName)) {
                mapperExtXmlGeneratorPlugin = (Plugin) Class.forName(mapperExtXmlGeneratorPluginClassName).getDeclaredConstructors()[0].newInstance(initargs);
                mapperExtXmlGeneratorPlugin.setContext(context);
                mapperExtXmlGeneratorPlugin.setProperties(new Properties());
            } else {
                mapperExtXmlGeneratorPlugin = null;
            }

            MmeCompileUtil.generate(context, modelPackageName, modelFileDir, exampleFileDir, 
                    mapperPackageName, mapperFileDir, xmlFileDir, 
                    mapperExtGeneratorPlugin, mapperExtXmlGeneratorPlugin);
        } catch (Exception e) {
            throw new MojoExecutionException("Generate MyBatis Model Example File Error!", e);
        }

        projectHelper.addResource(project, outputDirectory.getAbsolutePath(), 
                Arrays.asList("**/*.java"), Collections.emptyList());
        project.addCompileSourceRoot(outputDirectory.getAbsolutePath());
        buildContext.refresh(outputDirectory);
    }

}
